#' A param optim suite with which to run all the methods on the different datasets
#'
#' @param design Design tibble of the experiment, created by [paramoptim_generate_design()].
#' @param metrics Which metrics to evaluate; see [calculate_metrics()] for a list of which metrics are available.
#' @param qsub_params A list used to define execution parameters for each row in the design tibble.
#'
#'   * `memory` defines the amount of memory,
#'   * `timeout` defines the maximum wall time,
#'   * `num_cores` defines the number of cores
#'   * `num_iterations` defines the number of mlrmbo iterations
#'   * `num_init_params` defines the number of parameters in the iteration 0.
#'
#'   Optionally, a function in the format \code{function(XXX, YYY, ...) { ZZZ }} is possible, where XXX and YYY
#'   are equal to the groups defined by \code{qsub_grouping} (default \code{method_id} and \code{param_id}),
#'   and ZZZ is equal to some logic which always produces a \code{list(memory = ..., timeout = ..., num_cores ...)}.
#' @param qsub_grouping A character used to partition the design into separate jobs. Any of the column names
#'   in \code{design$crossing} is allowed to be used. This string will later be parsed by [glue::glue()].
#' @param verbose Whether or not to print extra information.
#' @param local_output_folder A folder in which to output intermediate and final results.
#' @param remote_output_folder A folder in which to store intermediate results in a remote directory when using the qsub package.
#'
#' @importFrom readr read_rds write_rds
#' @importFrom mlrMBO makeMBOControl setMBOControlTermination setMBOControlInfill makeMBOInfillCritDIB makeMBOInfillCritCB
#' @importFrom mlr makeLearner configureMlr
#' @importFrom parallelMap parallelStartMulticore parallelStop
#
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
#' design <- paramoptim_generate_design(
#'   datasets = datasets,
#'   methods = methods
#' )
#'
#' paramoptim_submit(
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
paramoptim_submit <- function(
  design,
  metrics = "correlation",
  qsub_grouping = "{method_id}/{prior_id}/{repeat_ix}",
  qsub_params = list(timeout = 3600, memory = "10G", num_cores = 8, num_iterations = 100, num_init_params = 39),
  verbose = TRUE,
  local_output_folder = derived_file("suite"),
  remote_output_folder = derived_file("suite", remote = TRUE)
) {
  requireNamespace("qsub")

  grouping_variables <-
    qsub_grouping %>%
    str_extract_all("\\{([^\\}]*)\\}") %>%
    .[[1]] %>%
    str_replace_all("[\\{\\}]", "")

  assert_that("method_id" %all_in% grouping_variables)

  paramoptim_submit_check(
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
    local_tmp_path = local_output_folder,
    remote_tmp_path = remote_output_folder
  )

  ## run evaluation for each method separately
  submit_method <- function (subcrossing) {
    subdesign <- subset_design(design, subcrossing)

    grouping_values <-
      subcrossing %>%
      select(one_of(grouping_variables)) %>%
      extract_row_to_list(1)

    # retrieve objects
    method_id <- as.character(subdesign$methods$id) %>% unique
    assert_that(
      length(method_id) == 1
    )
    method <- subdesign$methods %>% filter(id == !!method_id) %>% pull(fun) %>% first() %>% invoke()

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

    # set dynamic parameters
    if (is.function(qsub_params)) {
      qsub_params <- do.call(qsub_params, grouping_values)
    }

    ##################### MBO CONFIG ################################
    ## set settings for MBO parameter optimisation
    control <- mlrMBO::makeMBOControl(
      n.objectives = length(metrics),
      propose.points = qsub_params$num_cores,
      y.name = metrics
    ) %>%
      mlrMBO::setMBOControlTermination(
        iters = qsub_params$num_iterations
      )

    # Set infill criterion
    if (length(metrics) == 1) {
      control <- control %>%
        mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritCB())
    } else {
      control <- control %>%
        mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritDIB())
    }

    ## create learner for predicting performance of new params
    learner <- mlr::makeLearner(
      "regr.randomForest",
      se.method = "jackknife",
      predict.type = "se",
      keep.inbag = TRUE
    )

    # generate initial parameters
    par_set <- dynparam::as_paramhelper(method$parameters)
    ph_design <- bind_rows(
      ParamHelpers::generateDesignOfDefaults(par_set),
      ParamHelpers::generateDesign(n = qsub_params$num_init_params, par.set = par_set)
    )

    ##################### SUBMIT ################################
    qsub_config_method <-
      qsub::override_qsub_config(
        qsub_config = qsub_config,
        name = gsub("[\\/]", "_", dirname),
        num_cores = qsub_params$num_cores,
        memory = qsub_params$memory,
        max_wall_time = qsub_params$timeout,
        local_tmp_path = paste0(suite_method_folder, "/r2gridengine"),
        remote_tmp_path = paste0(suite_method_folder_rem, "/r2gridengine")
      )

    # which packages to load on the cluster
    qsub_packages <- c("dplyr", "purrr", "dyneval", "dynmethods", "readr", "dynbenchmark")

    # which data objects will need to be transferred to the cluster
    qsub_environment <-  c("metrics", "verbose", "subdesign", "control", "learner", "ph_design")

    # submit to the cluster
    qsub_handle <- qsub::qsub_lapply(
      X = 1,
      object_envir = environment(),
      qsub_environment = qsub_environment,
      qsub_packages = qsub_packages,
      qsub_config = qsub_config_method,
      FUN = paramoptim_qsub_fun
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
      learner,
      control
    )

    readr::write_rds(metadata, qsub_handle_file)

    invisible()
  }

  # run param optim per method seperately
  design$crossing <- design$crossing %>% mutate(., qsub_group = glue::glue_data(., qsub_grouping))
  runs <- design$crossing %>% split(., .$qsub_group)
  order <- design$crossing %>% pull(qsub_group) %>% unique()
  runs <- runs[order]

  walk(runs, submit_method)

  invisible()
}

#' @importFrom testthat expect_equal expect_is
#' @importFrom assertthat assert_that
paramoptim_submit_check <- function(
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
  assert_that(qsub_params %has_names% c("memory", "timeout", "num_cores", "num_iterations", "num_init_params"))

  # check timeout_per_execution
  assert_that(is.numeric(qsub_params[["timeout"]]))

  # check max_memory_per_execution
  assert_that(is.character(qsub_params[["memory"]]))
  assert_that(grepl("[0-9]+G", qsub_params[["memory"]]))
}


#' Helper function for parameter optimisation suite
#'
#' @param i Row of a design dataframe
paramoptim_qsub_fun <- function(i) {
  # call helper function
  paramoptim_run_evaluation(
    subdesign = subdesign,
    metrics = metrics,
    control = control,
    learner = learner,
    ph_design = ph_design,
    verbose = verbose
  )
}

#' @importFrom readr read_rds
#' @importFrom mlrMBO mbo
#' @importFrom mlr configureMlr
#' @importFrom parallelMap parallelStartMulticore parallelStop
paramoptim_run_evaluation <- function(
  subdesign,
  metrics,
  control,
  learner,
  ph_design,
  verbose
) {
  # objects
  dataset_ids <- as.character(subdesign$datasets$id)
  method_id <- as.character(subdesign$methods$id) %>% unique
  prior_id <- as.character(subdesign$priors$id) %>% unique

  assert_that(
    length(method_id) == 1,
    length(prior_id) == 1
  )
  datasets <- subdesign$datasets %>% filter(id %in% !!dataset_ids) %>% pull(fun) %>% map(invoke)
  method <- subdesign$methods %>% filter(id == !!method_id) %>% pull(fun) %>% first() %>% invoke()
  priors <- subdesign$priors %>% filter(id == prior_id) %>% pull(set) %>% first()

  # configure mlr
  mlr::configureMlr(show.learner.output = FALSE, on.learner.warning = "warn")

  # create an objective function
  obj_fun <- make_obj_fun(method = method, metrics = metrics, give_priors = priors, verbose = verbose)

  ## create a folder to save the intermediate mbo files in
  save_file_path <- paste0(getwd(), "/mlrmbo")
  dir.create(save_file_path, showWarnings = FALSE, recursive = TRUE)

  ## configure intermediate output
  control_train <- control
  control_train$save.file.path <- paste0(save_file_path, "/mlr_progress.RData")
  control_train$save.on.disk.at <- seq(0, control_train$iters+1, by = 1)

  ## start parallellisation
  parallelMap::parallelStartMulticore(cpus = control_train$propose.points, show.info = TRUE)

  ## Start training, and write intermediate results frequently to file
  mlrMBO::mbo(
    obj_fun,
    learner = learner,
    design = ph_design,
    control = control_train,
    show.info = TRUE,
    more.args = list(
      datasets = datasets
    )
  )

  ## stop parallellisation
  parallelMap::parallelStop()

  invisible()

}

#' Used for wrapping an evaluation function around a TI method
#'
#' @inheritParams dyneval::evaluate_ti_method
#' @param noisy Whether or not the metric is noisy or not
#' @param verbose Whether or not to print extra information output
#'
#' @importFrom smoof makeSingleObjectiveFunction makeMultiObjectiveFunction
#' @export
make_obj_fun <- function(method, metrics, give_priors, noisy = FALSE, verbose = FALSE) {
  # Use different makefunction if there are multiple metrics versus one
  if (length(metrics) > 1) {
    make_fun <- function(...) makeMultiObjectiveFunction(..., n.objectives = length(metrics))
  } else {
    make_fun <- makeSingleObjectiveFunction
  }

  # Wrap the method function in an evaluation function
  make_fun(
    name = "TItrain",
    vectorized = FALSE,
    minimize = metrics %in% c("rf_mse"),
    noisy = noisy,
    has.simple.signature = FALSE,
    par.set = dynparam::as_paramhelper(method$parameters),
    fn = function(x, datasets) {

      metric_names <- sapply(seq_along(metrics), function(i) {
        metric <- metrics[[i]]
        if (is.function(metric)) {
          names(metrics)[[i]]
        } else if (is.character(metric)) {
          metric
        } else {
          stop("Unexpected metric, check documentation.")
        }
      })

      eval_out <- evaluate_ti_method( # to do: why can't this be a list of datasets?
        dataset = list_as_tibble(datasets),
        method = method,
        parameters = x,
        give_priors = give_priors,
        metrics = metrics,
        output_model = FALSE,
        verbose = verbose
      )

      summary <- eval_out$summary

      # Calculate the final score
      score <-
        summary %>%
        summarise_at(metric_names, funs(mean)) %>%
        as.matrix %>%
        as.vector %>%
        set_names(metric_names)

      # post process evaluation output to get it
      # in the right format for mlrMBO
      attr(score, "extras") <- list(
        .summary = summary
      )

      score
    }
  )
}


