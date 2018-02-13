library(tidyverse)
library(dynutils)
library(PRISM)
#library(dyngen)

dataset_preprocessing("synthetic/v6")

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
updates_platform <- tibble(platform_name = list.files("ext_data/platforms/") %>% gsub("(.*)\\.rds", "\\1", .)) %>% filter(row_number() <= 10) %>% bind_rows(tibble(platform_name = "small"), .)

updates_replicates <- tibble(replicate_id = 1)
updates <- tidyr::crossing(updates_model, updates_platform, updates_replicates)
updates <- updates %>%
  group_by(modulenet_name) %>%
  mutate(dataset_id = paste0("synthetic/", modulenet_name, "_", seq_len(n()))) %>%
  ungroup()

update_params <- function(base_params=dyngen:::base_params, ...) {
  dots <- list(...)

  if("modulenet_name" %in% names(dots)) base_params$model$modulenet_name <- dots$modulenet_name
  if("totaltime" %in% names(dots)) base_params$simulation$totaltime <- dots$totaltime
  if("platform_name" %in% names(dots)) base_params$model$platform <- readRDS(paste0(find.package("dyngen"), "/ext_data/platforms/", dots$platform_name, ".rds"))
  if("platform_name" %in% names(dots)) base_params$experiment$platform <- readRDS(paste0(find.package("dyngen"), "/ext_data/platforms/", dots$platform_name, ".rds"))

  base_params$updates <- dots

  base_params
}

paramsets <- map(seq_len(nrow(updates)), function(row_id) {
  print(row_id)
  row <- dynutils::extract_row_to_list(updates, row_id)
  invoke(update_params, row)
})

# remote preparation
ncores <- 6
qsub_config <- override_qsub_config(num_cores = ncores, memory = paste0("10G"), wait=FALSE, r_module=NULL, execute_before="", name = "^_____^", stop_on_error = F, max_wall_time = "24:00:00")
qsub_config_single <- override_qsub_config(qsub_config, num_cores = 1)
qsub_packages <- c("tidyverse", "dyngen")

# creating folder structure locally and remote
folder <- remote_folder <- "/group/irc/shared/dynalysis/analysis/data/derived_data/datasets/synthetic/v6/"
folder <- "~/thesis/projects/dynverse/dynalysis/analysis/data/derived_data/datasets/synthetic/v6/"
# unlink(folder, recursive=TRUE);dir.create(folder, recursive=TRUE, showWarnings = FALSE)
# PRISM:::run_remote(glue::glue("rm -r {remote_folder}"), "prism")
# PRISM:::run_remote(glue::glue("mkdir {remote_folder}"), "prism")

# write_rds(paramsets, dataset_preproc_file("paramsets.rds"))
paramsets <- read_rds(dataset_preproc_file("paramsets.rds"))
PRISM:::rsync_remote("", folder, "prism", remote_folder)

# functions for get the folder for saving
model_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_model.rds")
simulation_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_simulation.rds")
gs_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_gs.rds")
gs_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_gs_plot.pdf")
experiment_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_experiment.rds")
experiment_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_experiment_plot.pdf")
normalisation_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_normalisation.rds")
normalisation_plot_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_normalisation_plot.pdf")
task_location <- function(folder, params_i) glue::glue("{folder}/{params_i}_task.rds")

# create remote environment
qsub_environment <- list2env(lst(paramsets, ncores, folder=remote_folder, model_location, simulation_location, gs_location, experiment_location, gs_plot_location, experiment_plot_location, normalisation_location, task_location, normalisation_plot_location))

params_i <- 2

#####################################
## Generate MODELS -------------------------
handle <- qsub_lapply(
  qsub_config=qsub_config_single %>% list_modify(name = "model"),
  qsub_environment=qsub_environment,
  qsub_packages=qsub_packages,
  seq_along(paramsets),
  function(params_i) {
    if (!file.exists(model_location(folder, params_i))) {
      print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
      params <- paramsets[[params_i]]
      options(ncores = 1)

      model <- invoke(dyngen:::generate_model_from_modulenet, params$model)
      model$uuids <- list(model=uuid::UUIDgenerate())
      saveRDS(model, model_location(folder, params_i))
    }
  }
) %>% write_rds(dataset_preproc_file("model_handle.rds"))
models <- qsub_retrieve(read_rds(dataset_preproc_file("model_handle.rds")))
PRISM:::rsync_remote("prism", remote_folder, "", folder)

## SIMULATE CELLS ---------------------------
# walk(seq_along(paramsets), function(params_i) {
qsub_lapply(
  qsub_config = qsub_config %>% list_modify(name = "simulation"),
  qsub_environment=qsub_environment,
  qsub_packages = qsub_packages,
  seq_along(paramsets),
  function(params_i) {
    if (!file.exists(simulation_location(folder, params_i))) {
      print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
      params <- paramsets[[params_i]]
      model <- readRDS(model_location(folder, params_i))
      options(ncores = ncores)

      simulation <- invoke(dyngen:::simulate_multiple, params$simulation, model$system)
      simulation$uuids <- list(simulation=uuid::UUIDgenerate()) %>% c(model$uuids)
      saveRDS(simulation, simulation_location(folder, params_i))
    }
    TRUE
  }
) %>% write_rds(dataset_preproc_file("simulations_handle.rds"))
simulations <- qsub_retrieve(read_rds(dataset_preproc_file("simulations_handle.rds")))
PRISM:::rsync_remote("prism", remote_folder, "", folder)

## Check simulations -------
PRISM::qsub_run(qsub_config = qsub_config %>% list_modify(name = "simulation_check", memory="15G"), qsub_environment=qsub_environment, qsub_packages = qsub_packages, function(i) {
  parallel::mclapply(seq_along(paramsets), function(params_i) {
    params <- paramsets[[params_i]]
    data <- list(exists = FALSE)
    if (file.exists(simulation_location(folder, params_i))) {
      data$exists <- TRUE
      simulation <- readRDS(simulation_location(folder, params_i))
      tibble(
        nsteps = nrow(simulation$stepinfo),
        params_i = params_i
      )
    }
  }, mc.cores=ncores)
}) %>% write_rds(dataset_preproc_file("simulations_checks_handle.rds"))
checks <- qsub_retrieve(read_rds(dataset_preproc_file("simulations_checks_handle.rds")))


# GOLD STANDARD ----------------------
# walk(seq_along(paramsets), function(params_i) {
qsub_lapply(
  qsub_config = qsub_config_single %>% list_modify(name = "gs", memory="30G"),
  qsub_environment=qsub_environment,
  qsub_packages = qsub_packages,
  seq_along(paramsets),
  function(params_i) {
    if (!file.exists(gs_location(folder, params_i))) {
      print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
      params <- paramsets[[params_i]]
      model <- readRDS(model_location(folder, params_i))
      simulation <- readRDS(simulation_location(folder, params_i))
      options(ncores = 1)

      print("Preprocessing")
      simulation <- dyngen:::preprocess_simulation_for_gs(simulation, model, params$gs$smooth_window) # do preprocessing separate, otherwise zoo will stay in an infinite loop in case of later error
      gs <- invoke(dyngen:::extract_goldstandard, params$gs, simulation, model, preprocess=FALSE)
      gs$checks <- dyngen:::check_goldstandard(gs)
      gs$uuids <- list(gs=uuid::UUIDgenerate()) %>% c(simulation$uuids)
      saveRDS(gs, gs_location(folder, params_i))
    }
  }
) %>% write_rds(dataset_preproc_file("gs_handle.rds"))
gs <- qsub_retrieve(read_rds(dataset_preproc_file("gs_handle.rds")))
PRISM:::rsync_remote("prism", remote_folder, "", folder)

checks <- map_dfr(seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  gs <- readRDS(gs_location(folder, params_i))

  tibble(all_represented = all(gs$checks$edge_counts > 0))
})

# EXPERIMENT ----------------------------------
# walk(seq_along(paramsets), function(params_i) {
handle <- qsub_lapply(qsub_config = qsub_config_single %>% list_modify(name = "experiment", memory="20G"), qsub_environment=qsub_environment, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  if (!file.exists(experiment_location(folder, params_i))) {
    library(tidyverse);library(dyngen)
    params <- paramsets[[params_i]]
    params$experiment %>% list2env(.GlobalEnv)

    simulation <- readRDS(simulation_location(folder, params_i))
    gs <- readRDS(gs_location(folder, params_i))
    options(ncores = 1)

    experiment <- invoke(dyngen:::run_experiment, params$experiment, simulation, gs)
    experiment$uuids <- list(experiment=uuid::UUIDgenerate()) %>% c(gs$uuids)
    saveRDS(experiment, experiment_location(folder, params_i))
  }
}) %>% write_rds(dataset_preproc_file("experiments_handle.rds"))
experiments <- qsub_retrieve(read_rds(dataset_preproc_file("experiments_handle.rds")))

# NORMALISE ----------------------------
qsub_lapply(qsub_config = qsub_config_single %>% list_modify(name = "normalisat"), qsub_environment=qsub_environment, qsub_packages = qsub_packages, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  if (!file.exists(normalisation_location(folder, params_i))) {
    params <- paramsets[[params_i]]
    experiment <- readRDS(experiment_location(folder, params_i))
    options(ncores = 1)

    graphics.off()
    pdf(normalisation_plot_location(folder, params_i), width=12, height=12)
    dev.control('enable')
    normalisation <- invoke(dynnormaliser::normalise_filter_counts, params$normalisation, experiment$counts, verbose = TRUE)
    normalisation$uuids <- list(normalisation=uuid::UUIDgenerate()) %>% c(experiment$uuids)
    saveRDS(normalisation, normalisation_location(folder, params_i))

    walk(normalisation$normalisation_plots, print)
    dyngen:::plot_normalisation(experiment, normalisation)
    graphics.off()
  }
}) %>% write_rds(dataset_preproc_file("normalisation_handle.rds"))
normalizations <- qsub_retrieve(read_rds(dataset_preproc_file("normalisation_handle.rds")))

# Plot GOLD STANDARD ----------------------------------------
qsub_lapply(qsub_config = qsub_config_single %>% list_modify(name = "gs_plot", memory="15G"), qsub_environment=qsub_environment, qsub_packages = qsub_packages, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  tryCatch({
    if (!file.exists(gs_plot_location(folder, params_i))) {
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
    }
  }, error=function(e) {print(params_i)}, finally={graphics.off()})
}) %>% write_rds(dataset_preproc_file("gs_plot_handle.rds"))
gs <- qsub_retrieve(read_rds(dataset_preproc_file("gs_plot_handle.rds")))

# Plot EXPERIMENT ----------------------------------------
params_i <- 3
qsub_lapply(qsub_config = qsub_config_single %>% list_modify(name = "exp_plot"), qsub_environment=qsub_environment, qsub_packages = qsub_packages, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  tryCatch({
    params <- paramsets[[params_i]]
    experiment <- readRDS(experiment_location(folder, params_i))

    graphics.off()
    pdf(experiment_plot_location(folder, params_i), width=12, height=12)
    dyngen:::plot_experiment(experiment)
    graphics.off()
  }, error=function(e) {print(params_i)}, finally={graphics.off()})
}) %>% write_rds(dataset_preproc_file("experiment_plot_handle.rds"))
experiment_plot <- qsub_retrieve(read_rds(dataset_preproc_file("experiment_plot_handle.rds")))

# Wrap into tasks ----------------------------
qsub_lapply(qsub_config = qsub_config_single %>% list_modify(name = "wrap", memory="20G"), qsub_environment=qsub_environment, qsub_packages = qsub_packages, seq_along(paramsets), function(params_i) {
  print(glue::glue("{params_i} / {length(paramsets)} ======================================"))
  if (!file.exists(task_location(folder, params_i))) {
    params <- paramsets[[params_i]]
    model <- readRDS(model_location(folder, params_i))
    simulation <- readRDS(simulation_location(folder, params_i))
    gs <- readRDS(gs_location(folder, params_i))
    experiment <- readRDS(experiment_location(folder, params_i))
    normalisation <- readRDS(normalisation_location(folder, params_i))

    task <- dyngen::wrap_task(params, model, simulation, gs, experiment, normalisation)
    task$uuids <- list(task=uuid::UUIDgenerate()) %>% c(normalisation$uuids)

    task %>% saveRDS(task_location(folder, params_i))
  }
}) %>% write_rds(dataset_preproc_file("wrap_handle.rds"))
wrap <- qsub_retrieve(read_rds(dataset_preproc_file("wrap_handle.rds")))

tasks <- dynutils::list_as_tibble(tasks)

write_rds(tasks, dataset_file("tasks.rds"))
tasks <- read_rds(dataset_file("tasks.rds"))

# tasks$id <- paste0("synthetic/", tasks$id)
# tasks$geodesic_dist <- pbapply::pblapply(seq_len(nrow(tasks)), function(i) {
#   dynutils::compute_tented_geodesic_distances(
#     dynutils::extract_row_to_list(tasks, i)
#   )
# })
# tasks$task_source <- "synthetic"


tasks <- list.files(folder, "*task.rds", full.names=T) %>% map(readRDS) %>% dynutils::list_as_tibble()
tasks$trajectory_type %>% table()
