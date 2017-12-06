library(tidyverse)
library(dynutils)
library(PRISM)
#library(dyngen)

updates_model <- tribble(
  ~modulenet_name, ~totaltime,
  "linear", 5,
  "bifurcating", 5,
  "linear_long", 30,
  "cycle", 30,
  "consecutive_bifurcating", 10,
  "bifurcating_converging", 15,
  "trifurcating", 10,
  "converging", 10,
  "bifurcating_loop", 30
)
updates_platform <- tribble(
  ~platform_name,
  "psc-astrocyte-maturation-neuron_sloan",
  "hematopoiesis-clusters_olsson"
)

updates_replicates <- tibble(replicate_id = 1)
updates <- tidyr::crossing(updates_model, updates_platform, updates_replicates)
updates <- updates %>%
  group_by(modulenet_name) %>%
  mutate(dataset_id = paste0(modulenet_name, "_", seq_len(n()))) %>%
  ungroup()

update_params <- function(base_params=dyngen:::base_params, ...) {
  dots <- list(...)

  if("modulenet_name" %in% names(dots)) base_params$model$modulenet_name <- dots$modulenet_name
  if("totaltime" %in% names(dots)) base_params$simulation$totaltime <- dots$totaltime
  if("platform_name" %in% names(dots)) base_params$experiment$platform <- readRDS(paste0(find.package("dyngen"), "/ext_data/platforms/", dots$platform_name, ".rds"))

  base_params$updates <- dots

  base_params
}

paramsets <- map(seq_len(nrow(updates)), function(row_id) {
  row <- dynutils::extract_row_to_list(updates, row_id)
  invoke(update_params, row)
})
# mutate <<- dplyr::mutate;filter <<- dplyr::filter

params <- paramsets[[1]]
params$experiment %>% list2env(.GlobalEnv)

# creating folder structure locally and remote
folder <- "~/thesis/projects/dynverse/dynalysis/analysis/data/derived_data/datasets/synthetic/v5/"
remote_folder <- "/group/irc/personal/wouters/projects/dynverse/dynalysis/analysis/derived_data/datasets/datasets/synthetic/v5/"
# unlink(folder);dir.create(folder, recursive=TRUE, showWarnings = FALSE)
# qsub_run(function(i) {unlink(remote_folder, recursive=TRUE);dir.create(remote_folder, recursive=TRUE, showWarnings = FALSE)}, qsub_environment=list2env(lst(remote_folder)))

saveRDS(paramsets, paste0(folder, "paramsets.rds"))

# functions for get the folder for saving
model_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_model.rds")
simulation_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_simulation.rds")
gs_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_gs.rds")
gs_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_gs_plot.pdf")
experiment_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_experiment.rds")
experiment_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_experiment_plot.pdf")
normalisation_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_normalisation.rds")
normalisation_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_normalisation_plot.pdf")

# remote preparation
ncores <- 3
qsub_config <- override_qsub_config(num_cores = ncores, memory = paste0("4G"), wait=FALSE)
qsub_environment <- list2env(lst(paramsets, ncores, folder=remote_folder, model_location, simulation_location, gs_location, experiment_location))

#####################################
## Generate MODELS ------------------------
models <- pbapply::pblapply(cl = 8, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  options(ncores = ncores)

  model <- invoke(dyngen:::generate_model_from_modulenet, params$model)
  saveRDS(model, model_location(folder, params_i))
})
PRISM:::rsync_remote("", folder, "prism", remote_folder)

## SIMULATE CELLS ---------------------------
# walk(seq_along(paramsets), function(params_i) {
handle <- qsub_lapply(qsub_config = qsub_config, qsub_environment=qsub_environment, qsub_packages = c("tidyverse"), seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  model <- readRDS(model_location(folder, params_i))
  options(ncores = ncores)

  simulation <- invoke(dyngen:::simulate_multiple, params$simulation, model$system)
  saveRDS(simulation, simulation_location(folder, params_i))
  TRUE
})

# GOLD STANDARD ----------------------
# walk(seq_along(paramsets), function(params_i) {
handle <- qsub_lapply(qsub_config = qsub_config, qsub_environment=qsub_environment, qsub_packages = c("tidyverse"), seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  model <- readRDS(model_location(folder, params_i))
  simulation <- readRDS(simulation_location(folder, params_i))
  options(ncores = ncores)

  print("Preprocessing")
  simulation <- dyngen:::preprocess_simulation_for_gs(simulation, model, params$gs$smooth_window) # do preprocessing separate, otherwise zoo will stay in an infinite loop in case of later error
  gs <- invoke(dyngen:::extract_goldstandard, params$gs, simulation, model, preprocess=FALSE)
  gs$checks <- dyngen:::check_goldstandard(gs)
  saveRDS(gs, gs_location(folder, params_i))
  TRUE
})
PRISM:::rsync_remote("prism", remote_folder, "", folder)

# EXPERIMENT ----------------------------------
walk(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  params$experiment %>% list2env(.GlobalEnv)

  simulation <- readRDS(simulation_location(folder, params_i))
  gs <- readRDS(gs_location(folder, params_i))
  options(ncores = ncores)

  experiment <- invoke(run_experiment, params$experiment, simulation, gs)
  saveRDS(experiment, experiment_location(folder, params_i))
  TRUE
})
filter <- dplyr::filter;mutate <- dplyr::mutate;arrange <- dplyr::arrange # stupid scater ruining our R environments -_-

# NORMALISE ----------------------------
walk(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  experiment <- readRDS(experiment_location(folder, params_i))
  options(ncores = ncores)

  experiment$counts %>% {log2(. + 1)} %>% apply(2, sd) %>% sort() %>% rev() %>% head(100) %>% names()

  normalisation <- invoke(dynutils::normalise_filter_counts, params$normalisation, experiment$counts, verbose=TRUE)
  saveRDS(normalisation, normalisation_location(folder, params_i))
  TRUE
})

# Plot GOLD STANDARD ----------------------------------------
options(ncores = 8)
params_i <- 3
walk(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  tryCatch({
    params <- paramsets[[params_i]]
    model <- readRDS(model_location(folder, params_i))
    simulation <- readRDS(simulation_location(folder, params_i))
    gs <- readRDS(gs_location(folder, params_i))

    graphics.off()
    pdf(gs_plot_location(folder, params_i), width=12, height=12)
    dyngen:::plot_net(model, label=FALSE, main_only = FALSE)
    dyngen:::plot_modulenet(model)
    dyngen:::plot_goldstandard(simulation, model, gs)
    graphics.off()
  }, error=function(e) {print(params_i)}, finally={graphics.off()})
})


# Plot EXPERIMENT ----------------------------------------
params_i <- 3
walk(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  tryCatch({
    params <- paramsets[[params_i]]
    experiment <- readRDS(experiment_location(folder, params_i))

    graphics.off()
    pdf(experiment_plot_location(folder, params_i), width=12, height=12)
    dyngen:::plot_experiment(experiment)
    graphics.off()
  }, error=function(e) {print(params_i)}, finally={graphics.off()})
})


# Plot NORMALISATION ----------------------------------------
params_i <- 3
walk(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  tryCatch({
    params <- paramsets[[params_i]]
    experiment <- readRDS(experiment_location(folder, params_i))
    normalisation <- readRDS(normalisation_location(folder, params_i))

    graphics.off()
    pdf(normalisation_plot_location(folder, params_i), width=12, height=12)
    dyngen:::plot_normalisation(experiment, normalisation)
    graphics.off()
  }, error=function(e) {print(glue::glue("error: {params_i}, {e}"))}, finally={graphics.off()})
})


# Wrap into tasks ----------------------------
tasks <- map(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  params <- paramsets[[params_i]]
  model <- readRDS(model_location(folder, params_i))
  simulation <- readRDS(simulation_location(folder, params_i))
  gs <- readRDS(gs_location(folder, params_i))
  experiment <- readRDS(experiment_location(folder, params_i))
  normalisation <- readRDS(normalisation_location(folder, params_i))

  dyngen::wrap_task(params, model, simulation, gs, experiment, normalisation)
})

tasks <- dynutils::list_as_tibble(tasks)

write_rds(tasks, "../dynalysis/analysis/data/derived_data/datasets/synthetic/v5.rds")
tasks <- read_rds("../dynalysis/analysis/data/derived_data/datasets/synthetic/v5.rds")
