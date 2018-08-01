library(dynbenchmark)
library(tidyverse)
library(dyntoy)

experiment("03-metric_characterisation/01-reference_vs_perturbation")

source(scripts_file("helper-01-perturbations.R"))

##  ............................................................................
##  Define metrics to test                                                  ####
real_metrics <- formals(calculate_metrics)$metrics %>% eval %>% as.list() %>% {set_names(., as.character(.))}

dummy_metrics <- list(
  perfect = function(dataset, model) 1,
  random = function(dataset, model) runif(1, 0, 1)
)

metrics <- c(
  real_metrics,
  dummy_metrics
)



datasets <- dyntoy::generate_dataset(model = "linear", num_cells = 100) %>% add_cell_waypoints()
perturbed <- dataset %>% perturb_switch_n_cells(10)

results <- dyneval::calculate_metrics(
  dataset,
  perturbed,
  metrics = metrics
)
scores <- results[names(metrics)]

scores
