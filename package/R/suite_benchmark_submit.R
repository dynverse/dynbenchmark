#' A benchmark suite with which to run all the methods on the different datasets
#'
#' @param design Design tibble of the experiment, created by [benchmark_generate_design()].
#' @param timeout_per_execution The maximum number of seconds each execution is allowed to run, otherwise it will fail.
#' @param max_memory_per_execution The maximum amount of memory each execution is allowed to use, otherwise it will fail.
#' @param metrics Which metrics to evaluate; see [calculate_metrics()] for a list of which metrics are available.
#' @param local_output_folder A folder in which to output intermediate and final results.
#' @param remote_output_folder A folder in which to store intermediate results in a remote directory when using the qsub package.
#' @param execute_before Shell commands to execute before running R.
#' @param verbose Whether or not to print extra information.
#'
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
  submit_method <- function (subcrossing) {
    subdesign <- subset_design(design, subcrossing)

    method_id <- subdesign$method$id[[1]]
    param_id <- subdesign$parameters$id[[1]]

    # check whether results already exist
    suite_method_folder <- file.path(local_output_folder, method_id, param_id)
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
      qsub_environment <-  c("metrics", "verbose", "subdesign")

      # submit to the cluster
      qsub_handle <- qsub::qsub_lapply(
        X = seq_len(nrow(subcrossing)),
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
        subdesign,
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
  runs <- design$crossing %>% split(., paste0(.$method_id, "_", .$param_id))
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
  # check datasets
  testthat::expect_true(all(c("id", "type", "fun") %in% colnames(design$datasets)))
  testthat::expect_is(design$datasets$id, "character")
  testthat::expect_false(any(duplicated(design$datasets$id)))
  testthat::expect_true(all(design$datasets$type %in% c("character", "dynwrap", "function")))
  testthat::expect_true(design$datasets$fun %>% map_lgl(is.function) %>% all)
  ix <- which(design$datasets$type == "character")
  testthat::expect_true(all(design$datasets$id[ix] %in% list_datasets()$dataset_id))

  # check methods
  testthat::expect_true(all(c("id", "type", "fun") %in% colnames(design$methods)))
  testthat::expect_is(design$methods$id, "character")
  testthat::expect_false(any(duplicated(design$methods$id)))
  testthat::expect_true(all(design$methods$type == "character" | !(design$methods$id %in% dynmethods::methods$id)))

  # check priors
  testthat::expect_true(all(c("id", "set") %in% colnames(design$priors)))
  testthat::expect_is(design$priors$id, "character")

  # check parameters
  testthat::expect_true(all(c("id", "method_id", "params") %in% colnames(design$parameters)))
  testthat::expect_is(design$parameters$id, "character")
  testthat::expect_false(any(duplicated(paste0(design$parameters$id, "_", design$parameters$method_id))))

  walkdf(
    design$parameters,
    function(l) {
      method_inputs <- design$methods %>% filter(id == l$method_id) %>% pull(fun) %>% .() %>% .$inputs %>% filter(type == "parameter") %>% pull(input_id)
      params <- l$params
      testthat::expect_true(all(names(parameters) %in% method_inputs))
    }
  )

  # check timeout_per_execution
  testthat::expect_is(timeout_per_execution, "numeric")

  # check max_memory_per_execution
  testthat::expect_is(max_memory_per_execution, "character")
  testthat::expect_match(max_memory_per_execution, "[0-9]+G")
}

subset_design <- function(design, subcrossing) {
  variable <- subcrossing %>% select(dataset_id, prior_id, repeat_ix)
  dataset_ids <- subcrossing$dataset_id %>% as.character() %>% unique
  prior_ids <- subcrossing$prior_id %>% as.character() %>% unique
  method_ids <- subcrossing$method_id %>% as.character() %>% unique
  param_ids <- subcrossing$param_id %>% as.character() %>% unique

  # construct a new design
  list(
    datasets = design$datasets %>% filter(id %in% dataset_ids),
    methods = design$methods %>% filter(id %in% method_ids),
    priors = design$priors %>% filter(id %in% prior_ids),
    parameters = design$parameters %>% filter(method_id %in% method_ids, id %in% param_ids),
    crossing = subcrossing
  )
}

#' Helper function for benchmark suite
#'
#' @param design_row Row of a design dataframe
benchmark_qsub_fun <- function(i) {
  # call helper function
  benchmark_run_evaluation(i, subdesign, metrics, verbose)
}

#' @importFrom readr read_rds
benchmark_run_evaluation <- function(
  i,
  subdesign,
  metrics,
  verbose,
  error_mode = FALSE
) {
  row <- subdesign$crossing %>% extract_row_to_list(i)

  # read dataset
  dataset_id <- as.character(row$dataset_id)
  dataset <- subdesign$datasets %>% filter(id == dataset_id) %>% pull(fun) %>% {.[[1]]()}

  # get method
  setup_singularity_methods()
  method_id <- as.character(row$method_id)
  if (identical(error_mode, FALSE)) {
    method <- subdesign$methods %>% filter(id == method_id) %>% pull(fun) %>% {.[[1]]()}
  } else {
    # create a method that will just return the error generated by qsub
    method <- create_ti_method("error", run_fun = function(...) message(error_mode))()
  }

  # get parameters
  param_id <- as.character(row$param_id)
  params <- design$parameters %>% filter(id == param_id, method_id == !!method_id) %>% pull(params) %>% .[[1]]

  # get priors
  prior_id <- as.character(row$prior_id)
  priors <- design$priors %>% filter(id == prior_id) %>% pull(set) %>% .[[1]]

  # start evaluation
  out <- evaluate_ti_method(
    dataset = dataset,
    method = method,
    parameters = params,
    metrics = metrics,
    give_priors = priors,
    output_model = TRUE,
    mc_cores = 1,
    verbose = TRUE
  )

  # create summary
  bind_cols(
    data_frame(method_id, dataset_id, param_id, prior_id, repeat_ix = row$repeat_ix),
    out$summary %>%
      mutate(error_message = ifelse(is.null(error[[1]]), "", error[[1]]$message)) %>%
      select(-error, -method_id, -method_name, -dataset_id), # remove duplicate columns with design row
    tibble(
      model = out$models
    )
  )
}
