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

          if (length(out) != 1 || !is.na(out)) {
            # hooray, the benchmark suite ran fine!
            out

          } else {
            # if qacct is empty or the correct taskid cannot be found, then
            # this job never ran
            if (is.null(qacct_out) || !any(qacct_out$taskid == i)) {
              qsub_error <- "Job cancelled by user"
            } else {
              qacct_filt <-
                qacct_out %>%
                filter(taskid == i) %>%
                arrange(desc(row_number_i)) %>%
                slice(1)

              qacct_memory <- process_qsub_memory(qacct_filt$maxvmem)
              qacct_exit_status <- qacct_filt$exit_status %>% str_replace(" +", " ")
              qacct_exit_status_number <- qacct_exit_status %>% str_replace(" .*", "")
              qacct_user_time <- qacct_filt$ru_stime %>% str_replace("s$", "") %>% as.numeric

              qsub_memory <- process_qsub_memory(qacct_filt$maxvmem)
              qsub_user_time <- qsub_handle$max_wall_time

              memory_messages <- c("cannot allocate vector of size", "MemoryError", "OOM when allocating tensor", "error writing to connection")
              is_memory_problem <- function(message) {
                any(map_lgl(memory_messages, ~grepl(., message)))
              }

              qsub_error <-
                if (qacct_exit_status_number %in% c("134", "139") || is_memory_problem(attr(out, "qsub_error"))) {
                  "Memory limit exceeded"
                } else if (qacct_exit_status_number %in% c("137", "140", "9", "64")) {
                  "Time limit exceeded"
                } else if (qacct_exit_status_number != "0") {
                  paste0("Error status ", qacct_exit_status, "\n", attr(out, "qsub_error"))
                } else {
                  attr(out, "qsub_error")
                }
            }

            # "rerun" the evaluation in error mode, in order to generate the expected output except with
            # the default fail-scores for each of the metrics
            benchmark_run_evaluation(
              i = i,
              subdesign = metadata$subdesign,
              metrics = metadata$metrics,
              verbose = FALSE,
              error_mode = qsub_error,
              output_models = metadata$output_models
            )
          }
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
  output <- as_tibble(map_df(files, function(output_metrics_file) {
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

  output$error_status <- purrr::pmap_chr(output, extract_error_status)

  output
}



#' Extract the error status from an evaluation
#'
#' @param stdout The standard output
#' @param stderr The standard error
#' @param error_message The error message
#' @param ... Other columns in the output, ignored
extract_error_status <- function(stdout, stderr, error_message, ...) {
  memory_messages <- c(
    "cannot allocate vector of size", # R
    "MemoryError", # python
    "OOM when allocating tensor", # tensorflow
    "nullptr != block->mem", # tensorflow,
    "std::bad_alloc", # tensorflow
    "Bus error", # stemid 2 -> clustexpr
    "what\\(\\):  Resource temporarily unavailable" # grandprix
  )
  is_memory_problem <- function(message) {
    any(map_lgl(memory_messages, ~grepl(., message)))
  }

  case_when(
    error_message == "Memory limit exceeded" ~ "memory_limit",
    is_memory_problem(stderr) ~ "memory_limit",
    is_memory_problem(error_message) ~ "memory_limit",
    is_memory_problem(stdout) ~ "memory_limit",
    stringr::str_detect(error_message, "Time limit exceeded") ~ "time_limit",
    stringr::str_detect(stderr, "Time limit exceeded") ~ "time_limit",
    stringr::str_detect(error_message, "^Error status [0-9]*.*") ~ "execution_error",
    stringr::str_detect(stderr, "^Error status [0-9]*.*") ~ "execution_error",
    nchar(error_message) > 0 ~ "method_error",
    TRUE ~ "no_error"
  )
}



process_qsub_memory <- function(qsub_memory) {
  if (str_detect(qsub_memory, "[0-9\\.]*MB")) {
    as.numeric(str_replace_all(qsub_memory, "([0-9\\.]*)MB", "\\1")) / 1024
  } else if (str_detect(qsub_memory, "[0-9\\.]*GB")) {
    as.numeric(str_replace_all(qsub_memory, "([0-9\\.]*)GB", "\\1"))
  } else {
    stop("Cannot process qsub memory: ", qsub_memory)
  }
}
