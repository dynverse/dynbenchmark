wrap <- function(milestone_network, progressions) {
  task <- dyneval::wrap_ti_prediction(
    "toy",
    "toy",
    cell_ids = unique(progressions$cell_id),
    milestone_ids = unique(c(milestone_network$from, milestone_network$to)),
    milestone_network=milestone_network,
    progressions = progressions
  )

  task$geodesic_dist <- compute_emlike_dist(task)

  task
}


generate_linear <- function(ncells=100) {
  milestone_network <- dyngen::generate_toy_milestone_network("linear")
  progressions <- dyngen::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}

generate_bifurcating <- function(ncells=100) {
  milestone_network <- dyngen::generate_toy_milestone_network("bifurcating")
  progressions <- dyngen::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}

generate_cycle <- function(ncells=100) {
  milestone_network <- dyngen::generate_toy_milestone_network("cycle")
  progressions <- dyngen::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}
