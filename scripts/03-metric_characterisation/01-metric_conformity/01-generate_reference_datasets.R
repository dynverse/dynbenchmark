## Generation of toy datasets used as reference for perturbations

library(tidyverse)
library(dynbenchmark)
library(dyntoy)

experiment("03-metric_characterisation/01-metric_conformity")

source(scripts_file("helper-topologies.R"))

# the topologies are defined here because they need to have the same number of edges for perturb_change_topology
dataset_design <-
  crossing(
    bind_rows(
      topologies %>% enframe("topology_id", "topology_model"),
      tribble(
        ~topology_id, ~topology_model,
        "bifurcation_simple", dyntoy::model_bifurcating(max_degree = 1)
      )
    ),
    num_cells = c(10, 50, 100, 200, 500),
    repeat_ix = 1
  ) %>%
  mutate(
    dataset_id = as.character(str_glue("{topology_id}_{num_cells}_{repeat_ix}"))
  )

datasets <- pmap(dataset_design, function(dataset_id, topology_model, num_cells, ...) {
  generate_dataset(
    id = dataset_id,
    model = topology_model,
    num_cells = num_cells,
    num_features = 20,
    allow_tented_progressions = FALSE,
    add_prior_information = FALSE,
    normalise = FALSE
  )
}) %>% set_names(dataset_design$dataset_id)
datasets_suite <- datasets %>% process_datasets_design()

write_rds(dataset_design, derived_file("dataset_design.rds"))
write_rds(datasets, derived_file("datasets.rds"))
