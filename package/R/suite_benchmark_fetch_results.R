#' Fetch the results of the benchmark jobs from the cluster.
#'
#' @param remote The host at which to check for running jobs.
#'   If \code{NULL}, each qsub handle will be checked individually.
#'   If \code{TRUE}, the default qsub handle will be used.
#'
#' @importFrom readr read_rds write_rds
#' @export
benchmark_fetch_results <- function(remote = NULL) {
  requireNamespace("qsub")

  local_output_folder <- derived_file("suite")

  # find all 2nd level folders with individual tasks
  handles <- list.files(local_output_folder, pattern = "qsubhandle.rds", recursive = TRUE, full.names = TRUE)

  # check for running job ids
  running_job_ids <-
    if (!is.null(remote)) {
      qsub::qstat_remote(remote = remote) %>%
      gsub("^ *([0-9]*).*", "\\1", .) %>%
      unique() %>%
      keep(~ . != "")
    } else {
      c()
    }

  # process each method separately
  map(handles, function(handle) {
    name <- handle %>% gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>% gsub("/qsubhandle.rds", "", ., fixed = TRUE)
    output_metrics_file <- gsub("qsubhandle.rds", "output_metrics.rds", handle, fixed = TRUE)
    output_models_file <- gsub("qsubhandle.rds", "output_models.rds", handle, fixed = TRUE)

    if (!file.exists(handle)) {
      cat(name, ": No qsub file was found.\n", sep = "")
      return(FALSE)
    }

    if (file.exists(output_metrics_file)) {
      cat(name, ": Output already present.\n", sep = "")
      return(FALSE)
    }

    cat(name, ": Attempting to retrieve output from cluster. ", sep = "")
    metadata <- readr::read_rds(handle)
    subdesign <- metadata$subdesign
    qsub_handle <- metadata$qsub_handle
    num_datasets <- qsub_handle$num_datasets

    if (qsub_handle$job_id %in% running_job_ids) {
      cat("Job is still running.\n")
      return(FALSE)
    }

    # attempt to retrieve results; return NULL if job is still busy or has failed
    output <- qsub::qsub_retrieve(
      qsub_handle,
      wait = FALSE
    )

    if (is.null(output)) {
      qstat_out <-
        tryCatch({
          qsub::qstat_j(qsub_handle)
        }, error = function(e) {
          NULL
        }, warning = function(w) {})
      if (is.null(qstat_out)) {
        cat("The job had finished but no output was found.\n")
      } else {
        cat("The job is still running.\n")
      }

      return(FALSE)
    }


    cat("Output found! Saving output.\n", sep = "")
    qacct_out <- qsub::qacct(qsub_handle)

    # process each task separately
    outputs <- map_df(seq_len(nrow(subdesign$crossing)), function(i) {
      out <- output[[i]]

      # if the method has errored and no data was generated at all,
      # try to find an error message and return it in the right format
      if (length(out) == 1 && is.na(out)) {
        stderr <- attr(out, "qsub_error")

        # if qacct is empty or the correct taskid cannot be found,
        # then this job never ran
        if (is.null(stderr) && (is.null(qacct_out) || !any(qacct_out$taskid == i))) {
          stderr <- "Job cancelled by user"
        }

        # use benchmark_run_evaluation to generate the expected output format and
        # errored metric scores
        out <- benchmark_run_evaluation(
          i = i,
          subdesign = metadata$subdesign,
          metrics = metadata$metrics,
          verbose = FALSE,
          error_mode = stderr,
          output_models = metadata$output_models
        )

        # invalidate timings for errored methods
        out <- out %>%
          mutate_at(vars(starts_with("time_")), function(x) NA)
      }

      qacct_variables <-
        qacct_out %>%
        filter(taskid == i) %>%
        arrange(desc(row_number_i)) %>%
        slice(1) %>%
        transmute(
          job_exit_status = exit_status %>% str_replace(" +", " ") %>% str_replace(" .*", ""),
          mem_io = qacct_process_memory(mem),
          disk_io = qacct_process_memory(io),
          max_mem = qacct_process_memory(maxvmem),
          mem_io_gb = mem_io / 1e9,
          disk_io_gb = disk_io / 1e9,
          max_mem_gb = max_mem / 1e9
        )

      if (nrow(qacct_variables) == 0) {
        error_message <- paste0("Task ", i, " is missing qacct information. This job was likely interrupted before the job was finished, and thus no qacct information is present.")
        warning(error_message)
        out$error_status <- "execution_error"
        out$error_message <- error_message
      } else {
        out <- bind_cols(out, qacct_variables)

        out$error_status <- extract_error_status(
          stdout = out$stdout,
          stderr = out$stderr,
          error_message = out$error_message,
          job_exit_status = out$job_exit_status,
          produced_model = ("model" %in% colnames(out)) && !is.null(out$model[[1]]) && !identical(out$model[[1]], FALSE)
        )
      }

      out
    })

    # save models separately
    if (metadata$output_models) {
      models <- outputs$model
      model_ids <- map_chr(models, function(model) {
        if (!is.null(model)) {
          model$id
        } else {
          NA
        }
      })
      models <- models %>% set_names(model_ids)
      outputs <- outputs %>% select(-model) %>% mutate(model_i = seq_len(n()), model_id = model_ids)
      readr::write_rds(models, output_models_file)
    }

    # save output
    readr::write_rds(outputs, output_metrics_file)

    return(TRUE)
  })

  # return nothing
  invisible()
}


extract_job_exit_status <- function(qacct_out, task_id) {
  if (!is.null(qacct_out) && any(qacct_out$taskid == task_id)) {
    qacct_out %>%
      filter(taskid == task_id) %>%
      arrange(desc(row_number_i)) %>%
      slice(1) %>%
      pull(exit_status) %>%
      str_replace(" +", " ") %>%
      str_replace(" .*", "")
  } else {
    NA
  }
}

qacct_process_memory <- function(str) {
  value <- str %>% gsub("[^0-9\\.]*", "", .) %>% as.numeric()
  exp <- str %>% gsub("[0-9\\.s]*", "", .)
  map <- c(
    "B" = 0,
    "KB" = 3,
    "MB" = 6,
    "GB" = 9,
    "TB" = 12,
    "PB" = 15,
    "EB" = 18,
    "ZB" = 21 # should be enough for now
  )
  value * 10^map[exp]
}


extract_error_status <- function(stdout, stderr, error_message, job_exit_status, produced_model) {
  txt <- tolower(paste0(stdout, " ", stderr, " ", error_message))
  memory_messages <- tolower(c(
    "cannot allocate vector of size", # R
    "MemoryError", # python
    "OOM when allocating tensor", # tensorflow
    "could not allocate memory", # ouija
    "nullptr != block->mem", # tensorflow,
    "std::bad_alloc", # tensorflow
    "Bus error", # stemid 2 -> clustexpr
    "what\\(\\):  Resource temporarily unavailable", # grandprix
    "Could not allocate metaspace", # cellrouter, something with "initialisation of vm"
    "space available for allocation", # paga
    "out of memory", # dpt
    "allocate working memory", # embeddr
    "error writing to connection"
  ))
  is_memory_problem <- function(message) {
    any(map_lgl(memory_messages, ~grepl(., message)))
  }

  case_when(
    produced_model ~ "no_error",
    job_exit_status %in% c("134", "139") ~ "memory_limit",
    is_memory_problem(txt) ~ "memory_limit",
    grepl("prior information", txt) ~ "missing_prior",
    job_exit_status %in% c("137", "140", "9", "64") ~ "time_limit",
    job_exit_status != "0" ~ "execution_error",
    TRUE ~ "method_error"
  )
}



#' Gather and bind the results of the benchmark jobs
#'
#' @param load_models Whether or not to load the models as well.
#' @param experiment_id The experiment_id, defaults to the current one
#' @param filter_fun A function with which to filter the data as it is being read from files. Function must take a single data frame as input and return a filtered data frame as a result.
#'
#' @importFrom readr read_rds
#' @export
benchmark_bind_results <- function(load_models = FALSE, experiment_id = NULL, filter_fun = NULL) {
  local_output_folder <- derived_file("suite", experiment_id = experiment_id)

  # find all 2nd level folders with individual tasks
  files <- list.files(local_output_folder, pattern = "output_metrics.rds", recursive = TRUE, full.names = TRUE)

  # process each method separately
  as_tibble(map_df(files, function(output_metrics_file) {
    name <- output_metrics_file %>%
      gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>%
      gsub("/output_metrics.rds", "", ., fixed = TRUE)

    output_models_file <- gsub("output_metrics.rds", "output_models.rds", output_metrics_file, fixed = TRUE)

    if (file.exists(output_metrics_file)) {
      cat(name, ": Reading previous output\n", sep = "")
      output <- readr::read_rds(output_metrics_file)

      if (!is.null(filter_fun)) {
        output$before_filter_index <- seq_len(nrow(output))
        output <- output %>% filter_fun()
      }

      # read models, if requested
      if (load_models && file.exists(output_models_file) && nrow(output) > 0) {
        models <- readr::read_rds(output_models_file)

        if (!is.null(filter_fun)) {
          models <- models[output$before_filter_index]
          output <- output %>% select(-before_filter_index)
        }

        output$model <- models
      }

      output
    } else {
      cat("Output not found, skipping", name, "\n", sep = "")
      NULL
    }
  }))
}


