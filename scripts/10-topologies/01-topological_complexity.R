library(dynbenchmark)
library(tidyverse)

experiment("10-topologies")

output <- benchmark_bind_results(load_models = TRUE, experiment_id = "04-method_testing") %>%
  filter(!map_lgl(model, is.null)) %>%
  select(method_id, dataset_id, model)

# simplify all milestone networks
simplify_milestone_network <- function(milestone_network) {
  milestone_network %>%
    igraph::graph_from_data_frame(directed = first(milestone_network$directed)) %>%
    dynwrap::simplify_igraph_network() %>%
    igraph::as_data_frame()
}

output$milestone_network <- output$model %>%
  map("milestone_network") %>%
  map(simplify_milestone_network)


# calculate milestone network statistics
calculate_milestone_network_statistics <- function(milestone_network) {
  lst(
      n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
      n_edges = nrow(milestone_network),
      complexity = n_nodes + n_edges
  )

  # classification <- dynwrap::classify_milestone_network(milestone_network)
  #
  # lst(
  #   trajectory_type = classification$network_type,
  #   n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
  #   n_edges = nrow(milestone_network)
  # ) %>% c(classification$properties)
}

prediction_statistics <- bind_cols(
  output %>% select(-milestone_network, -model),
  map_df(output$milestone_network, calculate_milestone_network_statistics)
)


# do the same for the datasets
design <- read_rds(derived_file("design.rds", "04-method_testing"))
datasets <- design$datasets %>%
  mutate(dataset = invoke_map(fun)) %>%
  select(dataset_id = id, dataset)
# datasets <- load_datasets(ids = unique(predictions$dataset_id))

datasets$milestone_network <- datasets$dataset %>%
  map("milestone_network") %>%
  map(simplify_milestone_network)

dataset_statistics <- bind_cols(
  datasets %>% select(-milestone_network, -dataset),
  map_df(datasets$milestone_network, calculate_milestone_network_statistics)
)

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
  ) %>%
  left_join(
    load_methods() %>% select(method_id = id, topology_inference),
    "method_id"
  )



##  ............................................................................
##  Overall ordering in complexity                                          ####

# determine order based on average
method_complexity_order <- statistics %>%
  group_by(method_id) %>%
  summarise(complexity_difference = mean(complexity_difference)) %>%
  ungroup() %>%
  arrange(complexity_difference) %>%
  pull(method_id)

plot_overall_complexity_difference <- statistics %>%
  mutate(method_id = factor(method_id, method_complexity_order)) %>%
  ggplot(aes(complexity_difference, method_id, fill = topology_inference)) +
  # geom_point() +
  ggridges::geom_density_ridges2() +
  geom_vline(xintercept = 0)

write_rds(plot_overall_complexity_difference, result_file("overall_complexity_difference.rds"))

##  ............................................................................
##  Trajectory type specific complexity                                     ####
