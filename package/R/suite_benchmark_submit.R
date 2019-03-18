#' A benchmark suite with which to run all the methods on the different datasets
#'
#' @param design Design tibble of the experiment, created by [benchmark_generate_design()].
#' @param metrics Which metrics to evaluate; see [calculate_metrics()] for a list of which metrics are available.
#' @param qsub_params A list used to define execution parameters for each row in the design tibble.
#'   \code{memory} is used to define the amount of memory, \code{timeout} is used to define the maximum wall time.
#'   Optionally, a function in the format \code{function(XXX, YYY, ...) { ZZZ }} is possible, where XXX and YYY
#'   are equal to the groups defined by \code{qsub_grouping} (default \code{method_id} and \code{param_id}),
#'   and ZZZ is equal to some logic which always produces a \code{list(memory = ..., timeout = ...)}.
#' @param qsub_grouping A character used to partition the design into separate jobs. Any of the column names
#'   in \code{design$crossing} is allowed to be used. This string will later be parsed by [glue::glue()].
#' @param verbose Whether or not to print extra information.
#' @param output_models Whether or not the model will be outputted.
#' @param local_output_folder A folder in which to output intermediate and final results.
#' @param remote_output_folder A folder in which to store intermediate results in a remote directory when using the qsub package.
#'
#' @importFrom readr read_rds write_rds
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tibble)
#'
#' datasets <- c("synthetic/dyntoy/bifurcating_1", "synthetic/dyntoy/bifurcating_2")
#' methods <- dynmethods::methods$method_id[1:3]
#'
#' design <- benchmark_generate_design(
#'   datasets = datasets,
#'   methods = methods
#' )
#'
#' benchmark_submit(
#'   design = design,
#'   metrics = c("correlation", "rf_mse"),
#'   qsub_grouping = "{method_id}/{replicate_ix}",
#'   qsub_params = function(method_id, replicate_ix) {
#'     params <- lst(memory = "10G", timeout = 3600)
#'     if (method_id == "scorpius") params$memory <- "5G"
#'     params
#'   }
#' )
#' }
benchmark_submit <- function(
  design,
  metrics = "correlation",
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = list(timeout = 3600, memory = "10G"),
  verbose = TRUE,
  output_models = TRUE,
  local_output_folder = derived_file("suite"),
  remote_output_folder = derived_file("suite", remote = TRUE)
) {
  requireNamespace("qsub")

  grouping_variables <- qsub_grouping %>%
    str_extract_all("\\{([^\\}]*)\\}") %>%
    .[[1]] %>%
    str_replace_all("[\\{\\}]", "")

  benchmark_submit_check(
    design = design,
    metrics = metrics,
    qsub_params = qsub_params,
    qsub_grouping = qsub_grouping
  )

  ## prepare for remote execution; create a qsub config
  qsub_config <- qsub::override_qsub_config(
    wait = FALSE,
    remove_tmp_folder = FALSE,
    stop_on_error = FALSE,
    verbose = verbose >= 2,
    num_cores = 1,
    local_tmp_path = local_output_folder,
    remote_tmp_path = remote_output_folder
  )

  ## run evaluation for each method separately
  submit_method <- function (subcrossing) {
    subdesign <- subset_design(design, subcrossing)

    grouping_values <- subcrossing %>%
      select(one_of(grouping_variables)) %>%
      extract_row_to_list(1)

    # check whether results already exist
    dirname <- with(grouping_values, glue::glue(qsub_grouping))
    suite_method_folder <- file.path(local_output_folder, dirname)
    suite_method_folder_rem <- file.path(remote_output_folder, dirname)
    output_file <- file.path(suite_method_folder, "output.rds")
    qsub_handle_file <- file.path(suite_method_folder, "qsubhandle.rds")

    qsub::mkdir_remote(path = suite_method_folder, remote = FALSE)

    if (file.exists(output_file) || file.exists(qsub_handle_file)) {
      return()
    }

    if (verbose) cat("Submitting ", dirname, "\n", sep = "")

    # set parameters for the cluster
    if (is.function(qsub_params)) {
      qsub_params <- do.call(qsub_params, grouping_values)
    }

    qsub_config_method <-
      qsub::override_qsub_config(
        qsub_config = qsub_config,
        name = gsub("[\\/]", "_", dirname),
        memory = qsub_params$memory,
        max_wall_time = qsub_params$timeout,
        local_tmp_path = paste0(suite_method_folder, "/r2gridengine"),
        remote_tmp_path = paste0(suite_method_folder_rem, "/r2gridengine")
      )

    # which packages to load on the cluster
    qsub_packages <- c("dplyr", "purrr", "dyneval", "dynmethods", "readr", "dynbenchmark")

    # which data objects will need to be transferred to the cluster
    qsub_environment <-  c("metrics", "verbose", "subdesign", "output_models")

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
      metrics,
      subdesign,
      grouping_variables,
      grouping_values,
      dirname,
      suite_method_folder,
      output_file,
      qsub_handle_file,
      qsub_params,
      qsub_handle,
      output_models
    )

    readr::write_rds(metadata, qsub_handle_file)

    invisible()
  }

  # run benchmark per method seperately
  design$crossing <- design$crossing %>% mutate(., qsub_group = glue::glue_data(., qsub_grouping))
  runs <- design$crossing %>% split(., .$qsub_group)
  order <- design$crossing %>% pull(qsub_group) %>% unique()
  runs <- runs[order]

  walk(runs, submit_method)

  invisible()
}

#' @importFrom testthat expect_equal expect_is
#' @importFrom assertthat assert_that
benchmark_submit_check <- function(
  design,
  metrics,
  qsub_grouping,
  qsub_params
) {
  check_design_datasets(design$datasets)
  assert_that(design$crossing$dataset_id %all_in% design$datasets$id)

  check_design_methods(design$methods)
  assert_that(design$crossing$method_id %all_in% design$methods$id)

  # check priors
  assert_that(design$priors %has_names% c("id", "set"))
  assert_that(is.character(design$priors$id))

  # check parameters
  assert_that(design$parameters %has_names% c("id", "method_id", "params"))
  assert_that(is.character(design$parameters$id))
  assert_that(!any(duplicated(paste0(design$parameters$id, "_", design$parameters$method_id))))

  # l <- design$parameters %>% extract_row_to_list(14)
  walkdf(
    design$parameters,
    function(l) {
      cat(l$method_id %>% as.character(), "\n", sep = "")
      passed_params <- l$params %>% names()
      if (length(passed_params) != 0) {
        method_description <-
          design$methods %>%
          filter(id == l$method_id) %>%
          pull(fun) %>%
          first() %>%
          invoke()
        method_params <- names(method_description$parameters)
        assert_that(passed_params %all_in% method_params)
      }
    }
  )

  # check grouping character
  assert_that(is.character(qsub_grouping))
  qsub_config_no_braces <- qsub_grouping %>% str_replace_all("\\{[^\\}]*\\}", "")

  assert_that(grepl("^[a-zA-Z_\\-\\.0-9/\\]*$", qsub_config_no_braces))

  grouping_variables <-
    qsub_grouping %>%
    str_extract_all("\\{([^\\}]*)\\}") %>%
    .[[1]] %>%
    str_replace_all("[\\{\\}]", "")
  assert_that(design$crossing %has_names% grouping_variables)

  # if qsub_params is a function, check it runs when provided all of these arguments
  if (is.function(qsub_params)) {
    assert_that(formalArgs(qsub_params) %all_in% grouping_variables)
    example <- design$crossing %>% slice(1) %>% select(one_of(grouping_variables)) %>% extract_row_to_list(1)
    qsub_params <- do.call(qsub_params, example)
  }


  # make sure it has the correct format
  assert_that(is.list(qsub_params))
  testthat::expect_equal(sort(names(qsub_params)), c("memory", "timeout"))

  # check timeout_per_execution
  assert_that(is.numeric(qsub_params[["timeout"]]))

  # check max_memory_per_execution
  assert_that(is.character(qsub_params[["memory"]]))
  testthat::expect_match(qsub_params[["memory"]], "[0-9]+G")
}

logical_error_wrapper <- function(expr, generate_error) {
  tryCatch({
    expr
    if (generate_error) {
      invisible()
    } else {
      TRUE
    }
  }, error = function(e) {
    if (generate_error) {
      stop(e)
    } else {
      FALSE
    }
  })
}

check_design_datasets <- function(datasets, generate_error = TRUE) {
  # check datasets
  logical_error_wrapper(generate_error = generate_error, expr = {
    assert_that(c("id", "type", "fun") %all_in% colnames(datasets))
    assert_that(is.character(datasets$id))
    assert_that(!any(duplicated(datasets$id)))
    assert_that(datasets$type %all_in% c("character", "dynwrap", "function"))
    assert_that(datasets$fun %>% map_lgl(is.function) %>% all)
    assert_that(all(datasets$type != "character" | datasets$id %in% list_datasets()$id))
  })
}

check_design_methods <- function(methods, generate_error = TRUE) {
  # check methods
  logical_error_wrapper(generate_error = generate_error, expr = {
    assert_that(c("id", "type", "fun") %all_in% colnames(methods))
    assert_that(is.character(methods$id))
    assert_that(!any(duplicated(methods$id)))
    assert_that(all(methods$type != "character" | methods$id %in% dynmethods::methods$method_id))
  })
}

subset_design <- function(design, subcrossing) {
  variable <- subcrossing %>% select(dataset_id, prior_id, repeat_ix)
  dataset_ids <- subcrossing$dataset_id %>% as.character() %>% unique
  prior_ids <- subcrossing$prior_id %>% as.character() %>% unique
  method_ids <- subcrossing$method_id %>% as.character() %>% unique

  # construct a new design
  out <- list(
    datasets = design$datasets %>% filter(id %in% dataset_ids),
    methods = design$methods %>% filter(id %in% method_ids),
    priors = design$priors %>% filter(id %in% prior_ids),
    crossing = subcrossing
  )

  if (!is.null(design$parameters)) {
    param_ids <- subcrossing$param_id %>% as.character() %>% unique
    out$parameters <- design$parameters %>% filter(method_id %in% method_ids, id %in% param_ids)
  }

  out
}

#' Helper function for benchmark suite
#'
#' @param i Row of a design dataframe
benchmark_qsub_fun <- function(i) {
  # call helper function
  benchmark_run_evaluation(
    i = i,
    subdesign = subdesign,
    metrics = metrics,
    verbose = verbose,
    error_mode = FALSE,
    output_models = output_models
  )
}

#' @importFrom readr read_rds
#' @importFrom dynwrap create_ti_method_r
benchmark_run_evaluation <- function(
  i,
  subdesign,
  metrics,
  verbose,
  error_mode = FALSE,
  output_models = TRUE
) {
  row <- subdesign$crossing %>% extract_row_to_list(i)

  # read dataset
  dataset_id <- as.character(row$dataset_id)
  method_id <- as.character(row$method_id)
  param_id <- as.character(row$param_id)
  prior_id <- as.character(row$prior_id)

  if (is.character(error_mode)) {
    # use a small dataset to error on
    dataset <- dyntoy::toy_datasets %>% slice(1)

    # create a method that will just return the error generated by qsub
    method <- dynwrap::create_ti_method_r(
      definition = dynwrap::definition(
        method = dynwrap::def_method(id = "error"),
        wrapper = dynwrap:def_wrapper(input_required = "expression")
      ),
      run_fun = function(expression, ..., seed = NULL, verbose = FALSE) message(error_mode),
      return_function = FALSE
    )

    params <- NULL

    priors <- NULL
  } else if (identical(error_mode, FALSE)) {
    # read dataset
    dataset <- subdesign$datasets %>% filter(id == !!dataset_id) %>% pull(fun) %>% first() %>% invoke()

    # get method
    method <- subdesign$methods %>% filter(id == !!method_id) %>% pull(fun) %>% first() %>% invoke()

    # get params
    params <- subdesign$parameters %>% filter(id == param_id, method_id == !!method_id) %>% pull(params) %>% first()

    # get priors
    priors <- subdesign$priors %>% filter(id == prior_id) %>% pull(set) %>% first()
  } else {
    stop("Invalid error_mode")
  }

  # start evaluation
  out <- evaluate_ti_method(
    dataset = dataset,
    method = method,
    parameters = params,
    metrics = metrics,
    give_priors = priors,
    output_model = output_models,
    verbose = verbose
  )

  # create summary
  error_helper <- function(x) {
    if (is.null(x) || is.na(x)) {
      ""
    } else if (is.list(x) && "message" %in% names(x)) { # is this still needed?
      x$message
    } else {
      x
    }
  }
  bind_cols(
    data_frame(method_id, dataset_id, param_id, prior_id, repeat_ix = row$repeat_ix),
    out$summary %>%
      mutate(error_message = error_helper(error[[1]])) %>%
      select(-error, -method_id, -method_name, -dataset_id), # remove duplicate columns with design row
    tibble(model = out$models)
  )
}
