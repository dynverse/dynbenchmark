#' Generation of toy datasets used as reference to assess metric conformity

library(tidyverse)
library(dynbenchmark)
library(dyntoy)

experiment("02-metrics/02-metric_conformity")

# construct all different combinations of topologies, # cells, ...
dataset_design <-
  crossing(
    bind_rows(
      dynbenchmark::topologies_with_same_n_milestones %>% enframe("topology_id", "topology_model") %>% mutate(allow_tented_progressions = FALSE),
      tribble(
        ~topology_id, ~topology_model, ~allow_tented_progressions,
        "bifurcation_simple", dyntoy::model_bifurcating(), FALSE,
        "bifurcation_tented", dyntoy::model_bifurcating(), TRUE
      )
    ),
    num_cells = c(10, 20, 50, 100, 200, 500),
    repeat_ix = 1,
    cell_positioning = c("edges", "milestones")
  ) %>%
  mutate(
    dataset_id = as.character(str_glue("{topology_id}_{num_cells}_{cell_positioning}_{repeat_ix}")),
    seed = repeat_ix
  )

dataset_design$dataset <- pmap(dataset_design, function(dataset_id, topology_model, num_cells, seed, cell_positioning, allow_tented_progressions, ...) {
  function() {
    set.seed(seed)

    dataset <- dyntoy::generate_dataset(
      id = dataset_id,
      model = topology_model,
      num_cells = num_cells,
      num_features = 200,
      allow_tented_progressions = allow_tented_progressions,
      add_prior_information = FALSE,
      normalise = FALSE
    )

    if (cell_positioning == "milestones") {
      dataset <- dataset %>% dynwrap::gather_cells_at_milestones()
    }

    dataset
  }
})

write_rds(dataset_design, derived_file("dataset_design.rds"))
