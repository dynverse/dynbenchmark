#' Fetch the results of the benchmark jobs from the cluster.
#'
#' @importFrom readr read_rds write_rds
#' @export
benchmark_fetch_results <- function() {
  requireNamespace("qsub")

  local_output_folder <- derived_file("suite")

  # find all 2nd level folders with individual tasks
  handles <- list.files(local_output_folder, pattern = "qsubhandle.rds", recursive = TRUE, full.names = TRUE)

  # process each method separately
  map(handles, function(qsubhandle_file) {
    name <- qsubhandle_file %>% gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>% gsub("/qsubhandle.rds", "", ., fixed = TRUE)
    output_metrics_file <- gsub("qsubhandle.rds", "output_metrics.rds", qsubhandle_file, fixed = TRUE)
    output_models_file <- gsub("qsubhandle.rds", "output_models.rds", qsubhandle_file, fixed = TRUE)

    # if the output has not been processed yet, but a qsub handle exists,
    # attempt to fetch the results from the cluster
    if (!file.exists(output_metrics_file) && file.exists(qsubhandle_file)) {
      cat(name, ": Attempting to retrieve output from cluster. ", sep = "")
      metadata <- readr::read_rds(qsubhandle_file)
      subdesign <- metadata$subdesign
      qsub_handle <- metadata$qsub_handle
      num_datasets <- qsub_handle$num_datasets

      # attempt to retrieve results; return NULL if job is still busy or has failed
      output <- qsub::qsub_retrieve(
        qsub_handle,
        wait = FALSE
      )

      if (!is.null(output)) {
        cat("Output found! Saving output.\n", sep = "")

        qacct_out <- qsub::qacct(qsub_handle)

        # process each job separately
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

          out$job_exit_status <- extract_job_exit_status(qacct_out, i)

          out$error_status <- extract_error_status(
            stdout = out$stdout,
            stderr = out$stderr,
            error_message = out$error_message,
            job_exit_status = out$job_exit_status,
            produced_model = ("model" %in% colnames(out)) && !is.null(out$model[[1]]) && !identical(out$model[[1]], FALSE)
          )

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

      } else {
        # the job is probably still running
        suppressWarnings({
          qstat_out <- qsub::qstat_j(qsub_handle)
        })

        error_message <-
          if (is.null(qstat_out) || nrow(qstat_out) > 0) {
            "job is still running"
          } else {
            "qsub_retrieve of results failed -- no output was produced, but job is not running any more"
          }

        cat("Output not found. ", error_message, ".\n", sep = "")
      }

      NULL
    } else {
      if (file.exists(output_metrics_file)) {
        cat(name, ": Output already present.\n", sep = "")
      } else {
        cat(name, ": No qsub file was found.\n", sep = "")
      }
      NULL
    }

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
#'
#' @importFrom readr read_rds
#' @export
benchmark_bind_results <- function(load_models = FALSE) {
  local_output_folder <- derived_file("suite")

  # find all 2nd level folders with individual tasks
  files <- list.files(local_output_folder, pattern = "output_metrics.rds", recursive = TRUE, full.names = TRUE)

  # process each method separately
  as_tibble(map_df(files, function(output_metrics_file) {
    name <- output_metrics_file %>% gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>% gsub("/output_metrics.rds", "", ., fixed = TRUE)
    output_models_file <- gsub("output_metrics.rds", "output_models.rds", output_metrics_file, fixed = TRUE)

    if (file.exists(output_metrics_file)) {
      cat(name, ": Reading previous output\n", sep = "")
      output <- readr::read_rds(output_metrics_file)

      # read models, if requested
      if (load_models && file.exists(output_models_file)) {
        models <- readr::read_rds(output_models_file)
        output$model <- models
      }

      output
    } else {
      cat("Output not found, skipping", name, "\n", sep = "")
      NULL
    }
  }))
}


