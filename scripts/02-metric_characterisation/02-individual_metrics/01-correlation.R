library(dynbenchmark)
library(tidyverse)

experiment("02-metric_characterisation/02-individual_metrics")

# select the most complex dataset of all our datasets (excluding disconnected)
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
  mutate(seed = repeat_ix)

# do the same for perturbations
perturb_shuffle_cells <- function(dataset, shuffle_perc, seed = 1) {
  set.seed(seed)

  shuffle_n <- min(round(length(dataset$cell_ids) * shuffle_perc), length(dataset$cell_ids))
  the_chosen_ones <- sample(dataset$cell_ids, shuffle_n)
  mapper <- set_names(dataset$cell_ids, dataset$cell_ids)
  mapper[match(the_chosen_ones, mapper)] <- sample(the_chosen_ones)

  progressions <- dataset$progressions %>%
    mutate(
      cell_id = mapper[cell_id]
    )
  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}

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
  set.seed(seed+1)
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

results <- bind_cols(
  bind_rows(scores),
  dataset_design
)

results$time <- results$time_waypointedgeodesic + results$time_correlation

true_correlation <- results %>%
  filter(n_cell_waypoints == max(n_cell_waypoints)) %>%
  group_by(shuffle_perc) %>%
  summarise(true_correlation = mean(correlation))

plot_correlation_distributions <- results %>%
  mutate_at(c("n_cell_waypoints", "shuffle_perc"), factor) %>%
  ggplot(aes(n_cell_waypoints, correlation, color = shuffle_perc)) +
  geom_hline(aes(yintercept = true_correlation, color = factor(shuffle_perc)), data = true_correlation, linetype = "dotted") +
  geom_boxplot(aes(group = factor(paste0(shuffle_perc, n_cell_waypoints))), position = position_identity(), outlier.shape = NA) +
  scale_y_continuous(label_metric("correlation", parse = TRUE), limits = c(0, 1)) +
  scale_x_discrete(label_long("n_cell_waypoints")) +
  scale_color_viridis_d(label_long("shuffle_perc"), labels = function(x) scales::percent(as.numeric(x))) +
  theme_pub()
plot_correlation_distributions

plot_mean_times <- mean_times %>%
  ggplot(aes(n_cell_waypoints, y = time)) +
    # geom_raster(aes(fill = log10(time))) +
    geom_bar(aes(y = time), stat = "identity") +
    geom_text(aes(label = label_time(time)), color = "white", vjust = 1.1) +
    # scale_fill_viridis_c(option = "A", end = 0.8) +
    scale_x_discrete(label_long("n_cell_waypoints")) +
    scale_y_continuous(expand = c(0, 0.1)) +
    theme_pub()


patchwork::wrap_plots(
  plot_mean_times,
  plot_correlation_distributions,
  ncol = 1,
  heights = c(1, 3)
)



plot_diff_vs_time <- results %>%
  left_join(true_correlation, "shuffle_perc") %>%
  mutate(diff_correlation = abs(true_correlation - correlation)) %>%
  ggplot(aes(time, diff_correlation, color = factor(n_cell_waypoints))) +
  geom_point() +
  theme_pub() +
  scale_x_log10()
plot_diff_vs_time

results %>%
  left_join(true_correlation, "shuffle_perc") %>%
  mutate(diff_correlation = true_correlation - correlation) %>%
  group_by(n_cell_waypoints) %>%
  summarise_all(funs(mean = mean, q90 = quantile(., 0.9), q10 = quantile(., 0.1))) %>%
  ggplot(aes(time_mean, diff_correlation_mean, color = factor(n_cell_waypoints))) +
    geom_point() +
    geom_errorbar(aes(ymax = diff_correlation_q90, ymin = diff_correlation_q10))




##  ............................................................................
##  Plots                                                                   ####
label_n_cell_waypoints <- function(x) {ifelse(is.na(x), "All cells", x)}

# plot time
dataset_design %>%
  filter(n_cell_waypoints %in% c(100, NA)) %>%
  ggplot(aes(factor(n_cell_waypoints), time)) +
  geom_line(aes(group = num_cells, color = factor(num_cells))) +
  scale_y_log10() +
  scale_x_discrete(labels = label_n_cell_waypoints) +
  scale_color_brewer(label_long("n_cells"), (palette = "YlOrRd")) +
  theme_pub() +
  labs(x = label_long("n_cell_waypoints"), y = label_long("time (seconds)"))

# plot distributions
dataset_design$distances_vector <- dataset_design$distances %>% map(as.numeric)
dataset_design %>%
  filter(num_cells == 1000) %>%
  unnest(distances_vector) %>%
  mutate(n_cell_waypoints = factor(n_cell_waypoints)) %>%
  slice() %>%
  ggplot(aes(x = distances_vector, y = n_cell_waypoints, fill = n_cell_waypoints)) +
  ggridges::geom_density_ridges() +
  labs(y = label_long("n_cell_waypoints"), x = label_long("geodesic_distance")) +
  scale_y_discrete(expand = c(0, 0), labels = label_long_cell_waypoints) +
  scale_fill_grey(guide = FALSE) +
  theme_pub()

# plot cor_cor
dataset_design %>%
  ggplot(aes(factor(n_cell_waypoints), cor_cor)) +
    geom_line(aes(group = repeat_ix)) +
    scale_x_discrete(label_long("n_cell_waypoints"), labels = label_n_cell_waypoints) +
    scale_y_continuous(label_long("correlation_between_correlation_of_cellular_distances"))

# plot correlation scatter
dataset_design %>%
  filter(num_cells == 1000, n_cell_waypoints %in% c(100, NA)) %>%
  mutate(geodesic_calculation = ifelse(is.na(n_cell_waypoints), "all_cells", "waypointed")) %>%
  mutate(cor_vector = map(cor, reshape2::melt)) %>%
  unnest(cor_vector) %>%
  select(geodesic_calculation, Var1, Var2, value) %>%
  spread(geodesic_calculation, value) %>%
  ggplot(aes(all_cells, waypointed)) + geom_point()


dataset_design$distances
