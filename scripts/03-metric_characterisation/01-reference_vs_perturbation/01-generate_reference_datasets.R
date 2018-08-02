## Generation of toy datasets used as reference for perturbations

library(tidyverse)
library(dynbenchmark)
library(dyntoy)

experiment("03-metric_characterisation/01-reference_vs_perturbation")

dataset_design <- crossing(
  tribble(
    ~topology_id, ~topology_model,
    "linear", model_linear(num_milestones = 5),
    "bifurcation", model_bifurcating(max_degree = 3)
  ),
  num_cells = c(2, 10, 50, 100, 200, 500)
) %>%
  mutate(
    dataset_id = str_glue("{topology_id}_{num_cells}")
  )

datasets_suite <- pmap(dataset_design, function(dataset_id, topology_model, num_cells, ...) {
  generate_dataset(
    unique_id = dataset_id,
    model = topology_model,
    allow_tented_progressions = FALSE,
    add_prior_information = FALSE,
    normalise = FALSE
  )
}) %>% process_datasets_design()

write_rds(dataset_design, derived_file("dataset_design.rds"))
write_rds(datasets_suite, derived_file("datasets_suite.rds"))
