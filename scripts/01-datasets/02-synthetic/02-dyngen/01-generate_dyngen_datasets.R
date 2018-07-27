library(qsub)
library(dyngen)
library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("synthetic/dyngen/linear_1")

generate_dyngen_dataset <- function(dataset_id, params) {
  dataset_preprocessing(dataset_id)

  model <- load_or_generate(
    dataset_source_file("model.rds"),
    invoke(dyngen::generate_model_from_modulenet, params$model)
  )

  simulation <- load_or_generate(
    dataset_source_file("simulation.rds"),
    invoke(dyngen::simulate_multiple, params$simulation, model$system)
  )

  gs <- load_or_generate(
    dataset_source_file("gs.rds"),
    invoke(dyngen::extract_goldstandard, params$gs, model = model, simulation = simulation)
  )

  experiment <- load_or_generate(
    dataset_source_file("experiment.rds"),
    invoke(dyngen::run_experiment, params$experiment, simulation=simulation, gs=gs)
  )

  normalisation <- load_or_generate(
    dataset_source_file("normalisation.rds"),
    invoke(dynnormaliser::normalise_filter_counts, params$normalisation, experiment$counts)
  )

  dataset <- load_or_generate(
    dataset_source_file("dataset.rds"),
    dyngen::wrap_dyngen_dataset(dataset_id, params, model, simulation, gs, experiment, normalisation)
  )
  dataset$dataset_source <- "synthetic/dyngen"
  dataset$simulation_design <- c(
    design_row,
    list(
      simulator = "dyngen",
      simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyngen","splatter", "dynbenchmark"))
    )
  )

  save_dataset(dataset, dataset_id)

  TRUE
}

# dataset_id <- "synthetic/dyngen/linear_1"
# params <- dyngen::simple_params
#
# generate_dyngen_dataset(dataset_id, params)

design <- crossing(
  modulenet_name = dyngen::list_modulenets(),
  platform = tibble(platform = load_platforms())
) %>%
  mutate(dataset_id = paste0("synthetic/dyngen/", as.character(row_number())))

design$params <- mapdf(design, function(design_row) {
  params <- dyngen::simple_params
  params$design_row <- design_row
  params$model$modulenet_name <- design_row$modulenet_name
  params$model$platform <- design_row$platform
  params$experiment$platform <- design_row$platform

  params
})

qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyngen", wait = F)
handle <- mapdf(design, identity) %>%
  qsub_lapply(
    function(design_row) {
      generate_dyngen_dataset(design_row$dataset_id, design_row$params)
    },
    qsub_config = qsub_config,
    qsub_environment = c("generate_dyngen_dataset", "load_or_generate"),
    qsub_packages = c("dynbenchmark", "tidyverse")
  )
