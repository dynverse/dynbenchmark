#' Calculate some statistics about the topologies predicted by the methods

library(dynbenchmark)
library(tidyverse)
library(furrr)
plan(multiprocess)

experiment("10-benchmark_interpretation")

# load in output models


# only take into account methods with free topology and tree detection
relevant_methods <- load_methods() %>% filter(source == "tool", topology_inference == "free", detects_tree) %>% pull(id)

output <- benchmark_bind_results(
  load_models = TRUE,
  local_output_folder = derived_file("suite", experiment_id = "06-benchmark"),
  filter_fun = function(tib) tib %>% filter(method_id %in% relevant_methods)
) %>%
  select(method_id, dataset_id, model, him) %>%
  filter(!map_lgl(model, is.null))

# simplify all milestone networks
simplify_milestone_network <- function(milestone_network) {
  milestone_network %>%
    igraph::graph_from_data_frame(directed = first(milestone_network$directed)) %>%
    dynwrap::simplify_igraph_network() %>%
    igraph::as_data_frame()
}

output$milestone_network <- output$model %>%
  map("milestone_network") %>%
  future_map(simplify_milestone_network)


# calculate milestone network statistics
calculate_milestone_network_statistics <- function(milestone_network) {
  lst(
    n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
    n_edges = nrow(milestone_network),
    complexity = n_nodes + n_edges
  )
}

prediction_statistics <- bind_cols(
  output %>% select(-milestone_network, -model),
  future_map_dfr(output$milestone_network, calculate_milestone_network_statistics)
)

# do the same for the datasets
datasets <- load_datasets(ids = unique(output$dataset_id))

datasets$milestone_network <- datasets$milestone_network %>%
  future_map(simplify_milestone_network)

dataset_statistics <- future_map_dfr(datasets$milestone_network, calculate_milestone_network_statistics) %>%
  mutate(dataset_id = datasets$id)
dataset_statistics$trajectory_type_dataset <- datasets$trajectory_type[match(dataset_statistics$dataset_id, datasets$id)]

# now combine and compare
statistics <- left_join(
  prediction_statistics,
  dataset_statistics,
  c("dataset_id"),
  suffix = c("_prediction", "_dataset")
)

# calculate some difference statistics and add information on the methods
statistics <- statistics %>%
  mutate(
    complexity_difference = complexity_prediction - complexity_dataset
  )

# add him
statistics$him <- output$him

# add means
statistics <- statistics %>%
  group_by(method_id) %>%
  mutate_if(is.numeric, funs(mean = mean)) %>%
  ungroup()

# and save the statistics
write_rds(statistics, derived_file("statistics.rds"))
