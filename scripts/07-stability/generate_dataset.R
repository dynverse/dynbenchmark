#' Helper function for generating a subsampled dataset

generate_dataset <- function(
  orig_dataset_id,
  pct_cells,
  pct_features,
  seed,
  cores = 1,
  verbose = TRUE
) {
  prev_seed <- .Random.seed[[1]]

  set.seed(seed)

  time0 <- Sys.time()

  base_traj <- dynbenchmark::load_dataset(orig_dataset_id)

  old_counts <- dynwrap::get_expression(base_traj, "counts")
  old_expression <- dynwrap::get_expression(base_traj, "expression")

  cell_ix <- sample.int(nrow(old_counts), pct_cells * nrow(old_counts))
  cell_names <- set_names(paste0("Cell", seq_along(cell_ix)), rownames(old_counts)[cell_ix])
  feature_ix <- sample.int(ncol(old_counts), pct_features * ncol(old_counts))
  feature_names <- set_names(paste0("Feature", seq_along(feature_ix)), colnames(old_counts)[feature_ix])

  counts <- old_counts[names(cell_names), names(feature_names)]
  expression <- old_expression[names(cell_names), names(feature_names)]
  dimnames(counts) <- dimnames(expression) <- list(cell_names %>% unname(), feature_names %>% unname())

  cell_id_map <- cell_names %>% enframe("old_id", "cell_id")
  feature_id_map <- feature_names %>% enframe("old_id", "feature_id")

  progressions <-
    base_traj$progressions %>%
    rename(old_id = cell_id) %>%
    right_join(cell_id_map, by = "old_id") %>%
    select(-old_id)

  set.seed(seed)
  dataset <-
    dynwrap::wrap_data(
      cell_ids = cell_names %>% unname(),
      cell_id_map = cell_id_map,
      feature_id_map = feature_id_map
    ) %>%
    dynwrap::add_trajectory(
      milestone_ids = base_traj$milestone_ids,
      milestone_network = base_traj$milestone_network,
      progressions = progressions,
      divergence_regions = base_traj$divergence_regions
    ) %>%
    dynwrap::add_expression(
      counts = counts,
      expression = expression
    ) %>%
    dynwrap::add_prior_information(
      verbose = verbose
    ) %>%
    dynwrap::add_cell_waypoints(100)

  set.seed(prev_seed)

  time1 <- Sys.time()

  if (verbose) cat("SCALINGDATASET: time elapsed ", round(as.numeric(difftime(time1, time0, units = "secs")), 2), " s\n", sep = "")

  dataset
}
