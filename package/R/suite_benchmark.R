check_benchmark_design_parameters <- function(
  datasets,
  methods,
  parameters,
  give_priors,
  num_repeats
) {
  if (!is.character(datasets)) {
    check <- is.list(datasets) && all(sapply(datasets, function(x) is.character(x) || is.function(x) || dynwrap::is_wrapper_with_expression(x)))
    if (!check) {
      stop("datasets is supposed be a vector of dataset ids, a list of dynwrap datasets, or a list of functions which will generate a dynwrap dataset")
    }
  }

  if (!is.character(methods)) {
    check <- is.list(methods) && all(sapply(methods, function(x) is.character(x) || dynwrap::is_ti_method(x)))
    if (!check) {
      stop("methods is supposed be a vector of methods ids, or a list of dynwrap ti methods")
    }
    method_names <- sapply(methods, function(x) if (is.character(x)) x else x$short_name)
  } else {
    method_names <- methods
  }

  testthat::expect_true(all(parameters$method_id %in% method_names))

  testthat::expect_true(is.numeric(num_repeats))
}

#' Generate a benchmarking design
#'
#' @param datasets The datasets to be used in the evaluation.
#'   Can be a character vector (the dataset ids), a list of
#'   dynwrap datasets, or a list of functions each of which will generate
#'   a dynwrap dataset.
#' @param methods The methods to be evaluated.
#'   Can be a character vector (the method ids), or a list of
#'   dynwrap TI methods.
#' @param parameters A named list containing data frames of the parameters to evaluate.
#'   The names of the list must be present in method_ids.
#'   The data frames must be of format \code{data_frame(paramset_id = "set1", param1 = "a", param2 = 2.0)}.
#' @param num_repeats The number of times to repeat the evaluation.
#' @inheritParams dynwrap::infer_trajectories
#'
#' @examples
#' library(tibble)
#' generate_benchmark_design(
#'   datasets = c("toy/bifurcating_1", "toy/bifurcating_2"),
#'   method_ids = c("angle", "scorpius", "tscan"),
#'   parameters = list(
#'     scorpius = tibble(paramset_ix = 1),
#'     tscan = tibble(paramset_ix = 1:3, clusternum_lower = 4:6, clusternum_upper = 18:20)
#'  ),
#'   give_priors = NULL,
#'   num_repeats = 2
#' )
#' @export
generate_benchmark_design <- function(
  datasets,
  methods,
  parameters = NULL,
  give_priors = NULL,
  num_repeats = 1
) {
  check_benchmark_design_parameters(
    datasets,
    methods,
    parameters,
    give_priors,
    num_repeats
  )

  method_names <- sapply(methods, function(x) if (is.character(x)) x else x$short_name)

  parameters <- parameters %>%
    group_by(method_id) %>%
    mutate(param_ix = seq_len(n())) %>%
    ungroup()

  param_df <- parameters %>%
    select(-paramset, -param_id) %>%
    mutate(
      method_id = match(method_id, method_names)
    )

  # generate designs of the different parts of the evaluation
  crossing <- crossing(
    dataset = seq_along(datasets),
    method = seq_along(methods),
    give_priors = seq_along(give_priors),
    num_repeats = seq_along(num_repeats)
  ) %>%
    left_join(param_df, by = c("method" = "method_id"))

  num_repeats <- seq_len(num_repeats)

  # all combinations of the different parts
  list(
    datasets = datasets,
    methods = methods,
    give_priors = give_priors,
    num_repeats = num_repeats,
    parameters = parameters,
    crossing = crossing
  )
}



#' A benchmark suite with which to run all the methods on the different datasets
#'
#' @param design Design tibble of the experiment, created by [generate_benchmark_design()].
#' @param timeout_per_execution The maximum number of seconds each execution is allowed to run, otherwise it will fail.
#' @param max_memory_per_execution The maximum amount of memory each execution is allowed to use, otherwise it will fail.
#' @param metrics Which metrics to evaluate; see \code{\link{calculate_metrics}} for a list of which metrics are available.
#' @param local_output_folder A folder in which to output intermediate and final results.
#' @param remote_output_folder A folder in which to store intermediate results in a remote directory when using the qsub package.
#' @param execute_before Shell commands to execute before running R.
#' @param verbose Whether or not to print extra information.
#'
#' @importFrom pbapply pblapply
#' @importFrom readr read_rds write_rds
#'
#' @export
benchmark_submit <- function(
  design,
  timeout_per_execution = 3600,
  max_memory_per_execution = "10G",
  metrics = "correlation",
  local_output_folder,
  remote_output_folder,
  execute_before = NULL,
  verbose = TRUE
) {
  requireNamespace("qsub")

  benchmark_submit_check(
    design,
    timeout_per_execution,
    max_memory_per_execution,
    metrics,
    local_output_folder,
    remote_output_folder,
    execute_before,
    verbose
  )

  ## prepare for remote execution; create a qsub config
  qsub_config <- qsub::override_qsub_config(
    wait = FALSE,
    remove_tmp_folder = FALSE,
    stop_on_error = FALSE,
    verbose = FALSE,
    num_cores = 1,
    memory = max_memory_per_execution,
    max_wall_time = timeout_per_execution,
    execute_before = execute_before,
    local_tmp_path = local_output_folder,
    remote_tmp_path = remote_output_folder
  )

  ## run evaluation for each method separately
  submit_method <- function (config) {
    datasets <- config$dataset
    config <- config %>% select(-dataset) %>% unique

    method_name <- design$methods[[settings$method]]
    if (!is.character(method_name)) method_name <- method_name$short_name

    params <- design$parameters %>% filter(method_id == method_name, param_ix == settings$param_ix)

    # check whether results already exist
    folder_name <- paste0(params$param_id, "_rep", config$num_repeats)
    suite_method_folder <- file.path(local_output_folder, method_name, folder_name)
    output_file <- file.path(suite_method_folder, "output.rds")
    qsubhandle_file <- file.path(suite_method_folder, "qsubhandle.rds")

    qsub::mkdir_remote(path = suite_method_folder, remote = FALSE)

    if (!file.exists(output_file) && !file.exists(qsubhandle_file)) {
      cat("Submitting ", method_id, "\n", sep = "")

      # set parameters for the cluster
      qsub_config_method <-
        qsub::override_qsub_config(
          qsub_config = qsub_config,
          name = paste0("D_", method_id),
          local_tmp_path = paste0(suite_method_folder, "/r2gridengine")
        )

      # which packages to load on the cluster
      qsub_packages <- c("dplyr", "purrr", "dyneval", "dynmethods", "readr")

      # which data objects will need to be transferred to the cluster
      qsub_environment <-  c("metrics", "verbose", "design", "config")

      # submit to the cluster
      qsub_handle <- qsub::qsub_lapply(
        X = datasets,
        object_envir = environment(),
        qsub_environment = qsub_environment,
        qsub_packages = qsub_packages,
        qsub_config = qsub_config_method,
        FUN = benchmark_qsub_fun
      )

      # save data and handle to RDS file
      metadata <- lst(
        local_output_folder,
        remote_output_folder,
        design,
        config,
        timeout_per_execution,
        max_memory_per_execution,
        metrics,
        suite_method_folder,
        output_file,
        qsubhandle_file,
        qsub_handle
      )
      readr::write_rds(metadata, qsubhandle_file)
    }
  }

  # run benchmark per method seperately
  runs <- design$crossing %>% split(., paste0(.$method, "_", .$param_ix, "_", .$num_repeats))
  for (i in seq_along(runs))  {
    submit_method(runs[[i]])
  }

  invisible()
}

#' @importFrom testthat expect_equal expect_is
benchmark_submit_check <- function(
  design,
  timeout_per_execution,
  max_memory_per_execution,
  metrics,
  local_output_folder,
  remote_output_folder,
  execute_before,
  verbose
) {
  # design should be generated by `generate_benchmark_design`
  # and should this always be of the correct format
  # (is this a little too optimistic?)

  # # check datasets
  # testthat::expect_true(all(design$dataset_id %in% list_datasets()$dataset_id))
  #
  # # check methods
  # testthat::expect_is(design$method_id, "character")
  # testthat::expect_true(all(design$method_id %in% dynmethods::methods$method_id))
  #
  # # check parameters
  # testthat::expect_true(all(map_lgl(design$parameters, is.list)))
  #
  # walk2(
  #   dynmethods::methods %>% slice(match(design$method_id, method_id)) %>% pull(inputs),
  #   design$parameters,
  #   function(inputs, parameters) {
  #     testthat::expect_true(all(
  #       names(parameters) %in% (inputs %>% filter(type == "parameter") %>% pull(input_id))
  #     ))
  #   }
  # )

  # check timeout_per_execution
  testthat::expect_is(timeout_per_execution, "numeric")

  # check max_memory_per_execution
  testthat::expect_is(max_memory_per_execution, "character")
  testthat::expect_match(max_memory_per_execution, "[0-9]+G")
}

#' Helper function for benchmark suite
#'
#' @param design_row Row of a design dataframe
benchmark_qsub_fun <- function(design_row) {
  # call helper function
  benchmark_run_evaluation(design_row, metrics, verbose, design, config)
}

#' @importFrom readr read_rds
benchmark_run_evaluation <- function(
  design_row,
  metrics,
  verbose,
  design,
  config,
  error_mode = FALSE
) {
  testthat::expect_is(design_row, "tbl")

  # read dataset
  dataset <- design$datasets[[design_row]]
  if (is.character(dataset)) {
    dataset <- load_dataset(dataset, as_tibble = TRUE)
    dataset_id <- dataset$id
  } else if (is.function(dataset)) {
    dataset <- dataset()
    dataset_id <- dataset$id
  } else if (dynwrap::is_data_wrapper(dataset)) {
    dataset_id <- dataset$id
  }

  # get method
  method <- design$methods[[config$method]]
  method_name <- ifelse(is.character(method), method, method$short_name)

  if (!dynwrap::is_ti_method(method) && is.character(method)) {
    setup_singularity_methods()
    requireNamespace("dynmethods")
    if (identical(error_mode, FALSE)) {
      method <- dynmethods::methods %>%
        slice(match(method, method_id)) %>%
        pull(fun_name) %>%
        get("package:dynmethods") %>%
        {.()}
    } else {
      # create a method that will just return the error generated by qsub
      method <- create_ti_method("error", run_fun = function(...) message(error_mode))()
    }
  }

  # get parameters
  param_id <- design$parameters %>%
    filter(method_id == method_name, param_ix == config$param_ix) %>%
    pull(param_id)
  params <- design$parameters %>%
    filter(method_id == method_name, param_ix == config$param_ix) %>%
    pull(paramset) %>%
    .[[1]]

  # start evaluation
  out <- evaluate_ti_method(
    datasets = dataset,
    method = method,
    parameters = params,
    metrics = metrics,
    output_model = TRUE,
    mc_cores = 1,
    verbose = TRUE
  )

  # create summary
  bind_cols(
    data_frame(method_id = method_name, dataset_id, param_id),
    out$summary %>%
      mutate(error_message = ifelse(is.null(error[[1]]), "", error[[1]]$message)) %>%
      select(-error, -method_id, -method_name, -dataset_id), # remove duplicate columns with design row
    tibble(
      model = out$models
    )
  )
}

#' Fetch the results of the benchmark jobs from the cluster.
#'
#' @param local_output_folder The folder in which to output intermediate and final results.
#'
#' @importFrom readr read_rds write_rds
#' @export
benchmark_fetch_results <- function(local_output_folder) {
  requireNamespace("qsub")

  method_ids <- list.dirs(local_output_folder, full.names = FALSE, recursive = FALSE) %>% discard(~ . == "")

  # process each method separately
  map(method_ids, function(method_id) {
    suite_method_folder <- paste0(local_output_folder, "/", method_id)
    output_metrics_file <- paste0(suite_method_folder, "/output_metrics.rds")
    output_models_file <- paste0(suite_method_folder, "/output_models.rds")
    qsubhandle_file <- paste0(suite_method_folder, "/qsubhandle.rds")

    # if the output has not been processed yet, but a qsub handle exists,
    # attempt to fetch the results from the cluster
    if (!file.exists(output_metrics_file) && file.exists(qsubhandle_file)) {

      cat(method_id, ": Attempting to retrieve output from cluster: ", sep = "")
      metadata <- readr::read_rds(qsubhandle_file)
      design_method <- metadata$design_method
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
        outputs <- map_df(seq_len(nrow(design_method)), function(design_row_ix) {
          design_row <- design_method[design_row_ix, ]
          out <- output[[design_row_ix]]

          if (length(out) != 1 || !is.na(out)) {
            # hooray, the benchmark suite ran fine!
            out

          } else {
            # if qacct is empty or the correct taskid cannot be found, then
            # this job never ran
            if (is.null(qacct_out) || !any(qacct_out$taskid == design_row_ix)) {
              qsub_error <- "Job cancelled by user"
            } else {
              qacct_filt <-
                qacct_out %>%
                filter(taskid == design_row_ix) %>%
                arrange(desc(row_number_i)) %>%
                slice(1)

              qacct_memory <- process_qsub_memory(qacct_filt$maxvmem)
              qacct_exit_status <- qacct_filt$exit_status %>% str_replace(" +", " ")
              qacct_exit_status_number <- qacct_exit_status %>% str_replace(" .*", "")
              qacct_user_time <- qacct_filt$ru_stime %>% str_replace("s$", "") %>% as.numeric

              qsub_memory <- process_qsub_memory(qacct_filt$maxvmem)
              qsub_user_time <- qsub_handle$max_wall_time

              memory_messages <- c("cannot allocate vector of size", "MemoryError", "OOM when allocating tensor")
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
            out <- benchmark_run_evaluation(
              design_row = design_row,
              metrics = metadata$metrics,
              verbose = FALSE,
              error_mode = qsub_error
            )
          }
        })

        # save models separately
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
        cat(method_id, ": Output already present.\n", sep = "")
      } else {
        cat(method_id, ": No qsub file was found.\n", sep = "")
      }
      NULL
    }

  })

  # return nothing
  invisible()
}


#' Gather and bind the results of the benchmark jobs
#'
#' @param local_output_folder The folder in which to output intermediate and final results.
#' @param load_models Whether or not to load the models as well.
#'
#' @importFrom readr read_rds
#' @export
benchmark_bind_results <- function(local_output_folder, load_models = FALSE) {
  method_ids <- list.dirs(local_output_folder, full.names = FALSE, recursive = FALSE) %>% discard(~ . == "")

  # process each method separately
  output <- as_tibble(map_df(method_ids, function(method_id) {
    suite_method_folder <- file.path(local_output_folder, method_id)
    output_metrics_file <- file.path(suite_method_folder, "output_metrics.rds")
    output_models_file <- file.path(suite_method_folder, "output_models.rds")

    if (file.exists(output_metrics_file)) {
      cat(method_id, ": Reading previous output\n", sep = "")
      output <- readr::read_rds(output_metrics_file)

      # read models, if requested
      if (load_models && file.exists(output_models_file)) {
        models <- readr::read_rds(output_models_file)
        output$model <- models
      }

      output
    } else {
      cat(method_id, ": Output not found, skipping\n", sep = "")
      NULL
    }
  }))

  output$error_status <- purrr::pmap_chr(output, extract_error_status)

  output
}



#' Extract the error status from an evaluation
#'
#' @param model The model
#' @param stdout The standard output
#' @param stderr The standard error
#' @param error_message The error message
#' @param ... Other columns in the output, ignored
extract_error_status <- function(model, stdout, stderr, error_message, ...) {
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
    is.null(model[[1]]) ~ "method_error",
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
