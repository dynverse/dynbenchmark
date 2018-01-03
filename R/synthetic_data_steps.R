## Generate models -------------------------
generate_model <- function(params) {
  prepare_environment()
  options(ncores = 1)

  model <- invoke(dyngen:::generate_model_from_modulenet, params$model)
  model$uuids <- list(model=uuid::UUIDgenerate())
  saveRDS(model, dataset_preproc_file(pritt("{params$settings$params_i}_model.rds")))
}

## Simulate -------------------------
simulate <- function(params) {
  prepare_environment()

  data <- load_data("model", params$settings$params_i)

  simulation <- invoke(dyngen:::simulate_multiple, params$simulation, data$model$system)
  simulation$uuids <- list(simulation=uuid::UUIDgenerate()) %>% c(data$model$uuids)
  saveRDS(simulation, dataset_preproc_file(pritt("{params$settings$params_i}_simulation.rds")))
}

## Check simulation -----------------
check_simulation <- function(params, params_i = params$settings$params_i) {
  prepare_environment()
  options(ncores = 1)

  checks <- list(exists = FALSE, params_i=params_i)

  simulation_location <- dataset_preproc_file(pritt("{params_i}_simulation.rds"))

  if (file.exists(simulation_location)) {
    data <- load_data("simulation", params_i)

    checks$exists <- TRUE
    simulation <- readRDS(simulation_location)

    checks$nsteps <- nrow(simulation$stepinfo)
  }

  checks
}

## Extract gold standard -----------------
extract_goldstandard <- function(params) {
  prepare_environment()

  data <- load_data(c("model", "simulation"), params$settings$params_i)

  simulation <- dyngen:::preprocess_simulation_for_gs(data$simulation, data$model, params$gs$smooth_window) # do preprocessing separate, otherwise zoo will stay in an infinite loop in case of later error
  gs <- invoke(dyngen:::extract_goldstandard, params$gs, simulation, data$model, preprocess=FALSE)
  gs$checks <- dyngen:::check_goldstandard(gs)
  gs$uuids <- list(gs=uuid::UUIDgenerate()) %>% c(data$simulation$uuids)

  saveRDS(gs, dataset_preproc_file(pritt("{params$settings$params_i}_gs.rds")))
}

## Generate experiment ------------
generate_experiment <- function(params) {
  prepare_environment()

  data <- load_data(c("simulation", "gs"), params$settings$params_i)

  experiment <- invoke(dyngen:::run_experiment, params$experiment, data$simulation, data$gs)
  experiment$uuids <- list(experiment=uuid::UUIDgenerate()) %>% c(data$gs$uuids)

  saveRDS(experiment, dataset_preproc_file(pritt("{params$settings$params_i}_experiment.rds")))
}

## Normalise ------------
normalise <- function(params) {
  prepare_environment()
  options(ncores = 1)

  data <- load_data(c("experiment"), params$settings$params_i)

  graphics.off()
  pdf(dataset_preproc_file(pritt("{params$settings$params_i}_normalisation_plot.pdf")), width=12, height=12)
  dev.control('enable')

  normalisation <- invoke(dynutils::normalise_filter_counts, params$normalisation, data$experiment$counts, verbose = TRUE)
  normalisation$uuids <- list(normalisation=uuid::UUIDgenerate()) %>% c(data$experiment$uuids)

  saveRDS(normalisation, dataset_preproc_file(pritt("{params$settings$params_i}_normalisation.rds")))

  walk(normalisation$normalisation_plots, print)
  dyngen:::plot_normalisation(data$experiment, normalisation)
  graphics.off()
}

## Plot gold standard -----------
plot_goldstandard <- function(params) {
  prepare_environment()

  data <- load_data(c("model", "simulation", "gs"), params$settings$params_i)

  gs_plot_location <- dataset_preproc_file(pritt("{params$settings$params_i}_gs_plot.pdf"))

  graphics.off()
  pdf(gs_plot_location, width=12, height=12)
  dyngen:::plot_net(model, label=FALSE, main_only = FALSE)
  dyngen:::plot_modulenet(model)
  dyngen:::plot_goldstandard(simulation, model, gs)
  graphics.off()
}

## Wrap -----------
wrap <- function(params) {
  data <- load_data(c("model", "simulation", "gs", "experiment", "normalisation"), params$settings$params_i)

  task <- dyngen::wrap_task(params, data$model, data$simulation, data$gs, data$experiment, data$normalisation)
  task$uuids <- list(task=uuid::UUIDgenerate()) %>% c(data$normalisation$uuids)

  task_location <- dataset_preproc_file(pritt("{params$settings$params_i}_task.rds"))
  task %>% saveRDS(task_location)
}
