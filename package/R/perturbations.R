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
