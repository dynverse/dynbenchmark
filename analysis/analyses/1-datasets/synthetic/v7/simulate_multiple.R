source("scripts/0_common.R")

paramsets <- readRDS(dataset_preproc_file("paramsets.rds"))

params <- paramsets[[1]]
params_i <- params$settings$params_i

# Helper function to load in data (model, simulation, ...) from disk
load_data <- function(load = character(), params_i = 1) {
  map(load, function(id) {
    readRDS(dataset_preproc_file(pritt("{params_i}_{id}.rds")))
  }) %>% set_names(load)
}

# Prepare environment for remote -----------------
ncores <- 6
prepare_environment <- function(ncores = 6) {
  library(tidyverse)
  library(dynalysis)
  
  options(ncores=ncores)
  
  dataset_preprocessing(dataset_id)
}
prepare_environment()

# Function for the different steps
source("scripts/2_steps.R")

## Run on cluster ------
qsub_environment <- list2env(lst(ncores, prepare_environment, dataset_id, load_data))
source("scripts/2_steps.R", local = qsub_environment)

qsub_config <- override_qsub_config(
  num_cores = ncores, 
  memory = paste0("10G"), 
  wait=FALSE, 
  r_module=NULL, 
  execute_before="", 
  name = "dyngen", 
  stop_on_error = F, 
  max_wall_time = "24:00:00"
)
qsub_config_single <- override_qsub_config(qsub_config, num_cores = 1)
qsub_packages <- c("tidyverse", "dynalysis", "dyngen")

generate_wrapper <- function(func, output_file, ncores) {
  function(params) {
    prepare_environment(ncores=ncores)
    if (!file.exists(dataset_preproc_file(pritt("{params$settings$params_i}_{output_file}")))) {
      func(params)
    } else {
      TRUE
    }
  }
}

run_cluster <- function(func, output_file, ncores, paramsets, qsub_config=qsub_config) {
  wrapper <- generate_wrapper(func, output_file, ncores)
  
  output_name <- gsub("(.*)\\..*", "\\1", output_file)
  
  handle <- qsub_lapply(
    qsub_config=qsub_config %>% list_modify(name = output_name), 
    qsub_environment=qsub_environment, 
    qsub_packages=qsub_packages,
    paramsets,
    wrapper
  )
  handle
}

run_local <- function(func, output_file, paramsets, ncores=6) {
  wrapper <- generate_wrapper(output_file, ncores)
  pblapply(
    paramsets,
    func
  )
}

## Run on cluster
ncores <- 6

model_handle <- run_cluster(generate_model, "model.rds", 1, paramsets, qsub_config_single)
simulate_handle <- run_cluster(simulate, "simulate.rds", ncores, paramsets, qsub_config)
check_simulation_handle <- run_cluster(check_simulation, "simulation_checks.rds", 1, paramsets, qsub_config_single %>% list_modify(memory="15G"))
gs_handle <- run_cluster(extract_goldstandard, "gs.rds", 1, paramsets, qsub_config_single %>% list_modify(memory="30G"))
run_cluster(generate_experiment, "experiment.rds", 1, paramsets, qsub_config_single %>% list_modify(memory="20G"))
normalise_handle <- run_cluster(normalise, "normalisation.rds", 1, paramsets, qsub_config_single)
run_cluster(plot_goldstandard, "gs_plot.rds", 1, paramsets, qsub_config_single %>% list_modify(memory="20G"))
run_cluster(plot_experiment, "experiment_plot.pdf", 1, paramsets, qsub_config_single)
wrap_handle <- run_cluster(wrap, "task.rds", 1, paramsets, qsub_config_single %>% list_modify(memory="20G"))

result <- qsub_retrieve(normalise_handle)
result

## Group tasks
tasks <- list.files(dataset_preproc_file(), "*task.rds", full.names=T) %>% sort() %>% map(readRDS) %>% dynutils::list_as_tibble()
tasks <- tasks %>% filter(map(settings, "platform_name") != "small")

write_rds(tasks, dataset_file("tasks.rds"))


PRISM:::rsync_remote("prism", paste0("/group/irc/shared/dynalysis", dataset_file(relative = T), "tasks.rds"), "", dataset_file())

tasks$trajectory_type %>% table()