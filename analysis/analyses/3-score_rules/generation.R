wrap <- function(milestone_network, ncells=100) {
  progressions <- dyntoy:::random_progressions(milestone_network, ncells)

  task <- dynutils::wrap_ti_prediction(
    "toy",
    "toy",
    cell_ids = unique(progressions$cell_id),
    milestone_ids = unique(c(milestone_network$from, milestone_network$to)),
    milestone_network=milestone_network,
    progressions = progressions
  )

  task$geodesic_dist <- dynutils:::compute_emlike_dist(task)

  task
}

generate_dataset <- function(trajectory_type="linear", ncells=100) {
  milestone_network <- dyntoy:::generate_toy_milestone_network(trajectory_type)
  wrap(milestone_network, ncells)
}
