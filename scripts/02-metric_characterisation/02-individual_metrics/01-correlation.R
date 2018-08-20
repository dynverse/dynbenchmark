library(dynbenchmark)
library(tidyverse)

original_dataset_design <- crossing(
  num_cells = 1000
)

original_dataset_design$dataset <- original_dataset_design %>% pmap(function(num_cells, ...)  {
  print(num_cells)
  dataset <- dyntoy::generate_trajectory(model = "bifurcating", num_cells = num_cells)
})

# add varying number of cell waypoints
dataset_design <- crossing(
  original_dataset_design,
  n_cell_waypoints = c(2, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 120, 150, 200, NA),
  repeat_ix = 1:3,
  seed = repeat_ix
)

dataset_design$dataset <- dataset_design %>% pmap(function(seed, dataset, n_cell_waypoints, ...)  {
  set.seed(seed)
  if (is.na(n_cell_waypoints)) {
    n_cell_waypoints <- length(dataset$cell_ids)
  }

  dataset %>% add_cell_waypoints(n_cell_waypoints)
})

# calculate geodesic distances
dataset_design$geodesic_distances <- map(dataset_design$dataset, function(x) {
  print(length(x$cell_ids))
  start <- Sys.time()
  distances <- dynwrap::compute_tented_geodesic_distances(x, waypoint_cells = x$waypoint_cells)
  time <- Sys.time() - start

  tibble(distances = list(distances), time = as.numeric(time))
})
dataset_design <- dataset_design %>% unnest(geodesic_distances)

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

perturbation_design <- dataset_design %>%
  mutate(
    perturbed = map(dataset, perturb_shuffle_cells, shuffle_perc = 0.5)
  )

scores <- map2_df(perturbation_design$dataset, perturbation_design$perturbed, dyneval::calculate_metrics, metrics = c("correlation"))

perturbation_scores <- perturbation_design %>% bind_cols(scores)

perturbation_scores %>%
  ggplot(aes(factor(n_cell_waypoints), correlation)) +
    geom_boxplot() +
    facet_wrap(~num_cells)

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
