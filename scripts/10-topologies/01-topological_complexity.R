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
  classification <- dynwrap::classify_milestone_network(milestone_network)

  lst(
      n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
      n_edges = nrow(milestone_network),
      complexity = n_nodes + n_edges,
      trajectory_type = classification$network_type
  )
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
  geom_vline(xintercept = 0) +
  theme_pub() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(labels = label_method)

plot_overall_complexity_difference

write_rds(plot_overall_complexity_difference, result_file("overall_complexity_difference.rds"))

##  ............................................................................
##  Trajectory type specific complexity                                     ####

method_ids <- c("slingshot", "paga", "monocle_ddrtree", "grandprix", "scorpius", "gng")

statistics_complexity <- bind_rows(
  statistics,
  statistics %>% mutate(trajectory_type_dataset = "all_trajectory_types")
)

bw <- 1.5
arrow_y <- 5
trajectory_type_colors <- c(set_names(trajectory_types$colour, trajectory_types$id), "all_trajectory_types" = "#333333")
complexity_difference_limits <- statistics_complexity %>% filter(method_id %in% method_ids) %>% pull(complexity_difference) %>% range()
arrow_annot_data <- tibble(
  method_id = factor(method_ids[[1]], method_ids),
  x = diff(complexity_difference_limits)/10 * c(-1, 1),
  text = c("Prediction too\nsimple", "Prediction too\ncomplex"),
  hjust = c(1, 0)
)

bind_rows(
  statistics,
  statistics %>% mutate(trajectory_type_dataset = "all_trajectory_types")
) %>%
  mutate(trajectory_type_dataset = factor(trajectory_type_dataset, names(trajectory_type_colors))) %>%
  filter(method_id %in% method_ids) %>%
  mutate(method_id = factor(method_id, method_ids)) %>%
  ggplot(aes(complexity_difference, trajectory_type_dataset)) +
  ggridges::geom_density_ridges(aes(fill = trajectory_type_dataset)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_text(aes(x = x, y = arrow_y, hjust = hjust, label = text), colour = "#333333", vjust = 1, lineheight = 0.8, size = 3.2, data = arrow_annot_data) +
  facet_grid(.~method_id, labeller = label_facet(label_method)) +
  scale_fill_manual(values = trajectory_type_colors, labels = label_long, guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(labels = label_long, expand = c(0, 0)) +
  theme_pub()

