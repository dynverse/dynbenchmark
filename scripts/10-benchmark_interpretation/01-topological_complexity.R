library(dynbenchmark)
library(tidyverse)
library(furrr)
plan(multiprocess)

experiment("10-benchmark_interpretation")

# load in output models
output <- benchmark_bind_results(load_models = TRUE, experiment_id = "06-benchmark") %>%
  select(method_id, dataset_id, model, him)

# only take into account methods with free topology and tree detection
relevant_methods <- load_methods() %>% filter(source != "control", topology_inference == "free", detects_tree) %>% pull(id)
output <- output %>%
  filter(method_id %in% relevant_methods) %>%
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
  # classification <- dynwrap::classify_milestone_network(milestone_network)

  lst(
      n_nodes = length(unique(c(milestone_network$from, milestone_network$to))),
      n_edges = nrow(milestone_network),
      complexity = n_nodes + n_edges#,
      # trajectory_type = classification$network_type
  )
}

prediction_statistics <- bind_cols(
  output %>% select(-milestone_network, -model),
  future_map_dfr(output$milestone_network, calculate_milestone_network_statistics)
)


# do the same for the datasets
datasets <- load_datasets(ids = unique(output$dataset_id))
datasets <- datasets

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

##  ............................................................................
##  Overall ordering in complexity                                          ####

# determine order based on average
method_order <- statistics %>% arrange(complexity_difference_mean) %>% pull(method_id) %>% unique()

max_complexity_difference <- 30.0
bandwidth <- 1

plot_overall_complexity_difference <- statistics %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  mutate(complexity_difference = case_when(
    complexity_difference > max_complexity_difference ~ max_complexity_difference,
    complexity_difference < -max_complexity_difference ~ -max_complexity_difference,
    TRUE ~ as.double(complexity_difference)
  )) %>%
  ggplot(aes(complexity_difference, method_id)) +
  ggridges::geom_density_ridges2(aes(fill = complexity_difference_mean), bandwidth = bandwidth) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_pub() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(labels = label_method) +
  scale_fill_distiller(palette = "BrBG", limits = c(-10, 10), oob = scales::squish)

plot_overall_complexity_difference

write_rds(plot_overall_complexity_difference, result_file("overall_complexity_difference.rds"))

##  ............................................................................
##  Overall ordering in him                                                 ####
method_order <- statistics %>% arrange(him_mean) %>% pull(method_id) %>% unique()

plot_overall_him <- statistics %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  ggplot(aes(him, method_id)) +
  ggridges::geom_density_ridges2(aes(fill = him_mean)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_pub() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(labels = label_method) +
  scale_fill_distiller(palette = "BrBG", limits = c(0, 1), oob = scales::squish)

plot_overall_him

write_rds(plot_overall_complexity_difference, result_file("overall_him.rds"))

##  ............................................................................
##  Trajectory type specific complexity                                     ####

method_ids <- c("slingshot", "paga_tree", "gng", "monocle_ddrtree", "projected_tscan")

# add a row "all trajectory types"
statistics_complexity <- bind_rows(
  statistics,
  statistics %>% mutate(trajectory_type_dataset = "all_trajectory_types")
)

# some parameters of the plot
bw <- 2
alpha <- 0.8
trajectory_type_colors <- c(set_names(dynwrap::trajectory_types$colour, dynwrap::trajectory_types$id), "all_trajectory_types" = "#333333")
arrow_y <- length(trajectory_type_colors) + 1

complexity_difference_clip <- 25
statistics_complexity$complexity_difference_clipped <- case_when(
  statistics_complexity$complexity_difference > complexity_difference_clip ~ complexity_difference_clip,
  statistics_complexity$complexity_difference < -complexity_difference_clip ~ -complexity_difference_clip,
  TRUE ~ as.double(statistics_complexity$complexity_difference)
)

complexity_difference_limits <- statistics_complexity %>%
  filter(method_id %in% method_ids) %>%
  pull(complexity_difference_clipped) %>%
  range() %>%
  {. + c(-bw, +bw)}


arrow_annot_data <- tibble(
  method_id = factor(method_ids[[1]], method_ids),
  x = complexity_difference_limits,
  text = c("Prediction too\nsimple", "Prediction too\ncomplex"),
  hjust = c(0, 1)
)

plot_topology_complexity <- statistics_complexity %>%
  mutate(trajectory_type_dataset = factor(trajectory_type_dataset, names(trajectory_type_colors))) %>%
  filter(method_id %in% method_ids) %>%
  mutate(method_id = factor(method_id, method_ids)) %>%
  ggplot(aes(complexity_difference_clipped, trajectory_type_dataset)) +
  ggridges::geom_density_ridges2(
    aes(fill = trajectory_type_dataset),
    bandwidth = bw,
    alpha = alpha,
    from = complexity_difference_limits[[1]],
    to = complexity_difference_limits[[2]]
  ) +
  # geom_point() +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(x = x, y = arrow_y, hjust = hjust, label = text), colour = "#333333", vjust = 1, lineheight = 0.8, size = 3.2, data = arrow_annot_data %>% mutate(arrow_y = arrow_y)) +
  facet_grid(.~method_id, labeller = label_facet(label_method)) +
  scale_fill_manual(values = trajectory_type_colors, labels = label_long, guide = "none") +
  scale_y_discrete(label_long("Reference trajectory type"), expand = c(0,0), labels = label_long) +
  scale_x_continuous(label_long("Difference in topology size (= # milestones + # edges)\nbetween prediction and reference"), expand = c(0, 0), limits = complexity_difference_limits) +
  theme_pub()

plot_topology_complexity

plot_topology_complexity %>% write_rds(result_file("topology_complexity.rds"))
