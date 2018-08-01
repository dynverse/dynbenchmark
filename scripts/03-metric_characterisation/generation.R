wrap <- function(milestone_network, progressions) {
  dataset <- dynutils::wrap_ti_prediction(
    "toy",
    "toy",
    cell_ids = unique(progressions$cell_id),
    milestone_ids = unique(c(milestone_network$from, milestone_network$to)),
    milestone_network = milestone_network,
    progressions = progressions
  )

  dataset$geodesic_dist <- dynutils:::compute_tented_geodesic_distances(dataset)

  dataset
}


generate_linear <- function(ncells = 100) {
  milestone_network <- dyntoy::generate_milestone_network("linear")
  progressions <- dyntoy::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}

generate_bifurcating <- function(ncells = 100) {
  milestone_network <- dyntoy::generate_milestone_network("bifurcating")
  progressions <- dyntoy::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}

generate_cycle <- function(ncells = 100) {
  milestone_network <- dyntoy::generate_milestone_network("cycle")
  progressions <- dyntoy::random_progressions(milestone_network, ncells)

  wrap(milestone_network, progressions)
}
