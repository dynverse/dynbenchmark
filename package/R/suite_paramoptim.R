#' A parameter optimisation suite
#'
#' @param dataset_ids The ids of the datasets to be used in the evaluation.
#' @param methods A tibble of TI methods to use, see [dynwrap::get_ti_methods()].
#' @param timeout_paramoptim The parameter optimisation timeout
#' @param max_memory_per_core The maximum amount of memory each core is allowed to use
#' @param num_cores The number of cores to use.
#' @param metrics Which metrics to evaluate; see [calculate_metrics()] for a list of which metrics are available.
#' @param num_repeats The number of times to repeat the evaluation.
#' @param num_iterations The number of iterations to run.
#' @param num_init_params The number of initial parameters to evaluate.
#' @param local_output_folder A folder in which to output intermediate and final results.
#' @param remote_output_folder A folder in which to store intermediate results in a remote directory when using the qsub package.
#' @param execute_before Shell commands to execute before running R.
#' @param verbose Whether or not to print extra information.
#'
#' @importFrom readr read_rds write_rds
#' @importFrom testthat expect_equal expect_is
#' @importFrom mlrMBO makeMBOControl setMBOControlTermination setMBOControlInfill makeMBOInfillCritDIB makeMBOInfillCritCB
#' @importFrom mlr makeLearner configureMlr
#' @importFrom parallelMap parallelStartMulticore parallelStop
#' @importFrom dynwrap get_ti_methods
#'
#' @export
paramoptim_submit <- function(
  dataset_ids,
  methods,
  timeout_paramoptim,
  max_memory_per_core,
  num_cores,
  metrics,
  num_repeats,
  num_iterations,
  num_init_params,
  local_output_folder,
  remote_output_folder,
  execute_before = NULL,
  verbose = FALSE
) {
  requireNamespace("qsub")
  requireNamespace("randomForest")

  paramoptim_submit_check(
    dataset_ids,
    methods,
    timeout_paramoptim,
    max_memory_per_core,
    metrics,
    num_repeats,
    num_iterations,
    num_init_params,
    local_output_folder,
    remote_output_folder,
    execute_before,
    verbose
  )

  ##################### MBO CONFIG ################################
  ## set settings for MBO parameter optimisation
  control <- mlrMBO::makeMBOControl(
    n.objectives = length(metrics),
    propose.points = num_cores,
    y.name = metrics
  ) %>%
    mlrMBO::setMBOControlTermination(
      iters = num_iterations
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

  ##################### PRISM CONFIG ################################
  ## prepare for remote execution; create a qsub config
  qsub_config <- qsub::override_qsub_config(
    wait = FALSE,
    remove_tmp_folder = FALSE,
    stop_on_error = FALSE,
    verbose = FALSE,
    num_cores = num_cores,
    memory = max_memory_per_core,
    max_wall_time = timeout_paramoptim,
    r_module = NULL,
    execute_before = execute_before,
    local_tmp_path = local_output_folder,
    remote_tmp_path = remote_output_folder
  )

  ## run evaluation for each method separately
  map(seq_len(nrow(methods)), function(methodi) {
    method <- dynutils::extract_row_to_list(methods, methodi)

    # determine where to store certain outputs
    method_folder <- paste0(local_output_folder, "/", method$short_name)
    output_file <- paste0(method_folder, "/output.rds")
    qsubhandle_file <- paste0(method_folder, "/qsubhandle.rds")

    qsub::mkdir_remote(path = method_folder, remote = FALSE)

    ## If no output or qsub handle exists yet
    if (!file.exists(output_file) && !file.exists(qsubhandle_file)) {
      cat("Submitting ", method$name, "\n", sep = "")

      ## create a grid for each of the datasets, paramsets, and repeats
      grid <- crossing(
        repeat_i = seq_len(num_repeats)
      )

      # generate initial parameters
      design <- bind_rows(
        ParamHelpers::generateDesignOfDefaults(method$par_set),
        ParamHelpers::generateDesign(n = num_init_params, par.set = method$par_set)
      )

      # set parameters for the cluster
      qsub_config_method <-
        qsub::override_qsub_config(
          qsub_config = qsub_config,
          name = paste0("D_", method$short_name),
          local_tmp_path = paste0(method_folder, "/r2gridengine")
        )

      # which packages to load on the cluster
      qsub_packages <- c("dplyr", "purrr", "dyneval", "mlrMBO", "parallelMap", "readr", "dynbenchmark")

      # which data objects will need to be transferred to the cluster
      qsub_environment <-  c(
        "grid", "remote_output_folder", "method", "metrics", "verbose",
        "num_cores", "control", "learner", "design", "dataset_ids"
      )

      # submit to the cluster
      qsub_handle <- qsub::qsub_lapply(
        X = seq_len(nrow(grid)),
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
        dataset_ids,
        method,
        timeout_paramoptim,
        max_memory_per_core,
        num_cores,
        metrics,
        num_repeats,
        num_iterations,
        num_init_params,
        grid,
        control,
        learner,
        method_folder,
        output_file,
        qsubhandle_file,
        qsub_handle
      )
      readr::write_rds(metadata, qsubhandle_file)

      NULL
    }
  })

  invisible()
}

#' @importFrom testthat expect_equal expect_is
paramoptim_submit_check <- function(
  dataset_ids,
  methods,
  timeout_paramoptim,
  max_memory_per_core,
  metrics,
  num_repeats,
  num_iterations,
  num_init_params,
  local_output_folder,
  remote_output_folder,
  execute_before,
  verbose
) {
  # check datasets
  # TODO: check whether all datasets are present, local and remote

  # check methods
  testthat::expect_is(methods, "tbl")

  # check timeout_per_execution
  testthat::expect_is(timeout_paramoptim, "numeric")

  # check max_memory_per_execution
  testthat::expect_is(max_memory_per_core, "character")
  testthat::expect_match(max_memory_per_core, "[0-9]+G")
}


#' Helper function for paramoptim suite
#'
#' @param grid_i paramoptim config index
paramoptim_qsub_fun <- function(grid_i) {
  # call helper function
  paramoptim_run_optimisation(
    grid,
    grid_i,
    dataset_ids,
    control,
    learner,
    design,
    method,
    metrics,
    num_cores,
    verbose
  )
}

#' @importFrom readr read_rds
#' @importFrom parallelMap parallelStartMulticore parallelStop
#' @importFrom mlrMBO mbo
#' @importFrom mlr configureMlr
paramoptim_run_optimisation <- function(
  grid,
  grid_i,
  dataset_ids,
  control,
  learner,
  design,
  method,
  metrics,
  num_cores,
  verbose
) {
  # configure mlr
  mlr::configureMlr(show.learner.output = FALSE, on.learner.warning = "warn")

  # read datasets
  datasets <- map(dataset_ids, load_dataset) %>% list_as_tibble()

  # create an objective function
  obj_fun <- make_obj_fun(method = method, metrics = metrics, verbose = verbose)

  ## create a folder to save the intermediate mbo files in
  save_file_path <- paste0(getwd(), "/mlrmbo")
  dir.create(save_file_path, showWarnings = FALSE, recursive = TRUE)

  ## configure intermediate output
  control_train <- control
  control_train$save.file.path <- paste0(save_file_path, "/mlr_progress_", grid_i, ".RData")
  control_train$save.on.disk.at <- seq(0, control_train$iters+1, by = 1)

  ## start parallellisation
  parallelMap::parallelStartMulticore(cpus = num_cores, show.info = TRUE)

  ## Start training, and write intermediate results frequently to file
  mlrMBO::mbo(
    obj_fun,
    learner = learner,
    design = design,
    control = control_train,
    show.info = TRUE,
    more.args = list(
      datasets = datasets,
      output_model = FALSE
    )
  )

  ## stop parallellisation
  parallelMap::parallelStop()

  invisible()
}


#' Fetch the results of the paramoptim jobs from the cluster.
#'
#' @param local_output_folder The folder in which to output intermediate and final results.
#'
#' @importFrom readr read_rds write_rds
#' @export
paramoptim_fetch_results <- function(local_output_folder) {
  requireNamespace("qsub")

  method_names <- list.dirs(local_output_folder, full.names = FALSE, recursive = FALSE) %>% discard(~ . == "")

  # process each method separately
  map(method_names, function(method_name) {
    method_folder <- paste0(local_output_folder, "/", method_name)
    output_metrics_file <- paste0(method_folder, "/output_parameters.rds")
    qsubhandle_file <- paste0(method_folder, "/qsubhandle.rds")

    # if the output has not been processed yet, but a qsub handle exists,
    # attempt to fetch the results from the cluster
    if (!file.exists(output_metrics_file) && file.exists(qsubhandle_file)) {

      cat(method_name, ": Attempting to retrieve output from cluster: ", sep = "")
      metadata <- readr::read_rds(qsubhandle_file)
      grid <- metadata$grid
      qsub_handle <- metadata$qsub_handle
      num_datasets <- qsub_handle$num_datasets

      # attempt to retrieve results; return NULL if job is still busy or has failed
      output <- qsub::qsub_retrieve(
        qsub_handle,
        wait = FALSE
      )

      ## Create summary from the last saved states
      summary <- map_df(seq_len(num_datasets), function(grid_i) {
        file <- paste0(qsub_handle$src_dir, "/mlrmbo/mlr_progress_", grid_i, ".RData")

        if (file.exists(file)) {
          load(file)
          opt.state$opt.problem$control$save.file.path <- file
          save(opt.state, file = file)
          mlr_out <- mlrMBO::mboFinalize(file)

          dob <- mlr_out$opt.path$env$dob
          path <- mlr_out$opt.path$env$path %>% select(-one_of(metadata$metrics))
          extra <- mlr_out$opt.path$env$extra

          map_df(seq_along(dob), function(param_i) {
            extra[[param_i]]$.summary  %>%
              mutate(
                repeat_i = grid$repeat_i[[grid_i]],
                grid_i,
                iteration = dob[[param_i]],
                param_i,
                param_row = list(path[param_i,,drop = F]),
                error_message = sapply(error, function(err) ifelse(is.null(err), "", err$message))
              ) %>%
              select(-error)
          })
        } else {
          # todo: mimic normal output
          NULL
        }
      })

      # save output
      readr::write_rds(summary, output_metrics_file)

      NULL
    } else {
      if (file.exists(output_metrics_file)) {
        cat(method_name, ": Output already present.\n", sep = "")
      } else {
        cat(method_name, ": No qsub file was found.\n", sep = "")
      }
      NULL
    }

  })

  # return nothing
  invisible()
}



#' Gather and bind the results of the paramoptim jobs
#'
#' @param local_output_folder The folder in which to output intermediate and final results.
#'
#' @importFrom readr read_rds
#' @export
paramoptim_bind_results <- function(local_output_folder) {
  method_names <- list.dirs(local_output_folder, full.names = FALSE, recursive = FALSE) %>% discard(~ . == "")

  # process each method separately
  as_tibble(map_df(method_names, function(method_name) {
    method_folder <- paste0(local_output_folder, method_name)
    output_metrics_file <- paste0(method_folder, "/output_parameters.rds")

    if (file.exists(output_metrics_file)) {
      cat(method_name, ": Reading previous output\n", sep = "")
      readr::read_rds(output_metrics_file)
    } else {
      cat(method_name, ": Output not found, skipping\n", sep = "")
      NULL
    }
  }))
}

#' Used for wrapping an evaluation function around a TI method
#'
#' @inheritParams dyneval::evaluate_ti_method
#' @param noisy Whether or not the metric is noisy or not
#' @param verbose Whether or not to print extra information output
#'
#' @importFrom smoof makeSingleObjectiveFunction makeMultiObjectiveFunction
#' @export
make_obj_fun <- function(method, metrics, noisy = FALSE, verbose = FALSE) {
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
    par.set = method$par_set,
    fn = function(x, datasets, output_model) {

      calc_metrics <- c(metrics, extra_metrics)
      calc_metrics <- calc_metrics[!duplicated(calc_metrics)]

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

      eval_out <- evaluate_ti_method(
        dataset = datasets,
        method = method,
        parameters = x,
        metrics = calc_metrics,
        output_model = output_model,
        verbose = verbose
      )

      summary <- eval_out$summary

      # Calculate the final score
      score <- summary %>%
        summarise_at(metric_names, funs(mean)) %>%
        as.matrix %>%
        as.vector %>%
        set_names(metric_names)

      # post process evaluation output to get it
      # in the right format for mlrMBO
      attr(score, "extras") <- list(
        .summary = summary,
        .models = eval_out$models
      )

      score
    }
  )
}

