#' Characterisation of the `r dynbenchmark::label_metric("correlation")`

library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("02-metrics/01-metric_characterisation")

#   ____________________________________________________________________________
#   The effect of the number of cell waypoints                              ####

dataset <- load_datasets(list_datasets() %>% filter(startsWith(source, "synthetic")) %>% pull(id)) %>%
  filter(trajectory_type != "directed_disconnected_graph") %>%
  mutate(n_edges = map_int(milestone_network, nrow)) %>%
  top_n(1, n_edges) %>%
  extract_row_to_list(1)

dataset_design <- crossing(
  n_cell_waypoints = c(2, 5, 10, 20, 50, 100, 200, 500, 1000),
  repeat_ix = 1:100,
  shuffle_perc = c(0, 0.2, 0.4, 0.6, 0.8, 1)
) %>%
  bind_rows(tibble(n_cell_waypoints = length(dataset$cell_ids), repeat_ix = 1, shuffle_perc = c(0, 0.2, 0.4, 0.6, 0.8, 1))) %>%
  mutate(seed = repeat_ix)

# do the same for perturbations
perturb_shuffle_cells <- dynbenchmark:::perturb_shuffle_cells

# perturb dataset and check correlation given a number of cell waypoints
check_correlation <- function(dataset, shuffle_perc, n_cell_waypoints, seed, ...) {
  set.seed(seed)
  perturbed <- perturb_shuffle_cells(dataset, shuffle_perc = shuffle_perc)

  # add cell waypoints
  if (is.na(n_cell_waypoints)) {
    n_cell_waypoints <- length(dataset$cell_ids)
  }
  set.seed(seed)
  dataset <- dataset %>% add_cell_waypoints(n_cell_waypoints)


  # add cell waypoints
  if (is.na(n_cell_waypoints)) {
    n_cell_waypoints <- length(perturbed$cell_ids)
  }
  set.seed(seed + 1)
  perturbed <- perturbed %>% add_cell_waypoints(n_cell_waypoints)

  # calculate correlation
  dyneval::calculate_metrics(dataset, perturbed, metrics = "correlation")
}

# run the experiment on qsub
qsub_config <- qsub::override_qsub_config(
  num_cores = 1,
  memory = "4G",
  batch_tasks = 10,
  wait = FALSE,
  max_wall_time = "03:00:00"
)
handle <- qsub_pmap(
  dataset_design,
  check_correlation,
  dataset = dataset,
  qsub_config = qsub_config,
  qsub_packages = c("dynbenchmark", "tidyverse"),
  qsub_environment = c("perturb_shuffle_cells")
)

save(
  handle,
  dataset,
  dataset_design,
  file = derived_file("01-correlation.rda")
)

##
load(derived_file("01-correlation.rda"))

scores <- qsub::qsub_retrieve(handle) %>% bind_rows()

save(
  scores,
  dataset,
  dataset_design,
  file = derived_file("01-correlation.rda")
)

##
load(derived_file("01-correlation.rda"))

results <- bind_cols(
  bind_rows(scores),
  dataset_design
) %>%
  mutate_at(c("n_cell_waypoints", "shuffle_perc"), factor) %>%
  mutate(n_cell_waypoints = n_cell_waypoints %>% forcats::fct_recode(all = last(levels(n_cell_waypoints))))

results$time <- results$time_waypointedgeodesic + results$time_correlation


##  ............................................................................
##  Plot correlation distributions vs running time                          ####
true_correlation <- results %>%
  filter(n_cell_waypoints == "all") %>%
  group_by(shuffle_perc) %>%
  summarise(true_correlation = mean(correlation))

plot_correlation_distributions <- results %>%
  ggplot(aes(n_cell_waypoints, correlation, color = shuffle_perc)) +
  geom_hline(aes(yintercept = true_correlation, color = shuffle_perc), data = true_correlation, linetype = "dotted") +
  geom_boxplot(aes(group = factor(paste0(shuffle_perc, n_cell_waypoints))), position = position_identity(), outlier.shape = NA) +
  scale_y_continuous(label_metric("correlation", parse = TRUE), limits = c(0, 1), breaks = round(true_correlation$true_correlation, 2)) +
  scale_x_discrete(label_long("n_cell_waypoints")) +
  scale_color_viridis_d(label_long("shuffle_perc"), labels = function(x) scales::percent(as.numeric(x))) +
  theme_pub()
plot_correlation_distributions

mean_times <- results %>%
  group_by(n_cell_waypoints) %>%
  summarise(time = mean(time))

plot_mean_times <- mean_times %>%
  ggplot(aes(n_cell_waypoints, y = time)) +
  # geom_raster(aes(fill = log10(time))) +
  geom_bar(aes(y = time), stat = "identity") +
  geom_text(aes(label = label_time(time)), color = "black", vjust = -0.1) +
  # scale_fill_viridis_c(option = "A", end = 0.8) +
  scale_x_discrete(label_long("n_cell_waypoints")) +
  scale_y_continuous(label_long("Time (seconds)"), expand = expand_scale(mult = c(0, 0.1)), breaks = round(range(mean_times$time))) +
  theme_pub()

plot_correlation_distributions_overview <- wrap_plots(
  plot_mean_times + theme_empty_x_axis,
  plot_correlation_distributions,
  ncol = 1,
  heights = c(1, 3)
)


##  ............................................................................
##  Plot datasets                                                           ####
set.seed(9)
dataset <- dyntoy::generate_dataset(model = dyntoy::model_binary_tree(num_branchpoints = 2)) %>%
  simplify_trajectory() %>%
  add_cell_waypoints(num_cells_selected = length(dataset$cell_ids)/2)

trajectory_positions <- with(
  dataset,
  dynwrap::determine_cell_trajectory_positions(milestone_ids, milestone_network, milestone_percentages, progressions, divergence_regions)
)

waypoint_cells <- dataset$waypoint_cells

plot_datasets <- wrap_plots(
  plot_graph(dataset) + ggtitle("Dataset"),
  plot_graph(
    dataset,
    grouping = trajectory_positions %>% select(cell_id, type) %>% deframe() %>% label_long()
  ) + ggtitle("Cell positions"),
  plot_graph(
    dataset,
    grouping = tibble(cell_id = dataset$cell_ids) %>% mutate(type = case_when(cell_id %in% waypoint_cells ~ "waypoint", TRUE ~ "not_waypoint")) %>% deframe() %>% label_long()
  ) + ggtitle("Waypoint cells"),
  ncol = 1
)

##  ............................................................................
##  Combine plots                                                           ####
plot_waypoints_overview <- wrap_plots(
  plot_datasets %>% wrap_elements(),
  plot_correlation_distributions_overview %>% wrap_elements(),
  nrow = 1
) + plot_annotation(tag_levels = "a")

plot_waypoints_overview

write_rds(plot_waypoints_overview, result_file("waypoints_overview.rds"))

ggsave(result_file("waypoints_overview.pdf"), width = 12, height = 8)


#   ____________________________________________________________________________
#   Reconstructing the trajectory from the geodesic distances?              ####

datasets <- dyntoy::generate_datasets(allow_tented_progressions = FALSE, num_features = 2, num_cells = 500, add_prior_information = FALSE, num_replicates = 1)

plots_geodesic_distance_dimreds <- mapdf(datasets, function(dataset) {
  distances <- dynwrap::calculate_geodesic_distances(dataset) %>% t()
  distances[is.infinite(distances)] <- max(distances[!is.infinite(distances)])

  dimred <- dyndimred::dimred_mds(distances)
  dimred %>% as.data.frame() %>%  ggplot(aes(comp_1, comp_2)) +
    geom_point() +
    theme_graph() +
    ggtitle(label_long(dataset$model)) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
})

plot_geodesic_distance_dimreds <- wrap_plots(plots_geodesic_distance_dimreds)

plot_geodesic_distance_dimreds

write_rds(plot_geodesic_distance_dimreds, result_file("geodesic_distances_dimreds.rds"))
ggsave(result_file("geodesic_distances_dimreds.pdf"), width = 12, height = 8)

