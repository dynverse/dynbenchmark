library(dynbenchmark)
library(tidyverse)
library(dynplot)

dataset_ids <- list_datasets() %>% pull(id)

for (i in seq_along(dataset_ids)) {
  id <- dataset_ids[[i]]
  cat(i, "/", length(dataset_ids), ": ", id, "\n", sep = "")
  dataset <- load_dataset(id)

  # dataset <- dataset %>% add_cell_waypoints()

  # dataset$dataset_source <- NULL
  # dataset$source <- gsub("/[^/]*$", "", dataset$id)

  # # fix count and expression functions
  for (col in c("expression", "counts")) {
    env <- new.env(baseenv())
    assign("id", id, env)
    assign("col", col, env)
    dataset[[col]] <- function() {
      readr::read_rds(dynbenchmark::dataset_file(paste0(col, ".rds"), id = id))
    }
    environment(dataset[[col]]) <- env
  }

  # dataset <- dataset %>% dynwrap::add_prior_information()

  # # rewrap dataset
  # dataset <- with(
  #   dataset,
  #   wrap_data(
  #     id = id,
  #     cell_ids = cell_ids,
  #     cell_info = cell_info,
  #     source = source,
  #     cell_grouping = cell_grouping,
  #     normalisation_info = normalisation_info,
  #     creation_date = creation_date
  #   ) %>% add_trajectory(
  #     milestone_ids = milestone_ids,
  #     milestone_network = milestone_network,
  #     divergence_regions = divergence_regions,
  #     progressions = progressions
  #   ) %>% add_expression(
  #     counts = counts,
  #     expression = expression,
  #     feature_info = feature_info
  #   ) %>% dynwrap::add_prior_information()
  # )

  # dataset$trajectory_type <- dynwrap::classify_milestone_network(dataset$milestone_network)$network_type
  # dataset$prior_information <- dynwrap::generate_prior_information(dataset$milestone_ids, dataset$milestone_network, dataset$progressions, dataset$milestone_percentages, dataset$counts, dataset$feature_info, dataset$cell_info)
  # dataset$progressions <- with(dataset, dynutils::convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages))

  write_rds(dataset, dataset_file(id = id, filename = "dataset.rds"), compress = "xz")
}
