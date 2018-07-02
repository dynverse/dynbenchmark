library(dynbenchmark)
library(tidyverse)
library(dynplot)

dataset_ids <- list_datasets()

for (i in seq_along(dataset_ids)) {
  dataset_id <- dataset_ids[[i]]
  cat(i, "/", length(dataset_ids), ": ", dataset_id, "\n", sep = "")
  dataset <- load_dataset(dataset_id)

  # rewrap dataset
  dataset <- with(
    dataset,
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      task_source = "real",
      cell_grouping = cell_grouping,
      normalisation_info = normalisation_info,
      creation_date = creation_date
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions
    ) %>% add_expression(
      counts = counts,
      expression = expression,
      feature_info = feature_info
    ) %>% dynwrap::add_prior_information()
  )

  # dataset$trajectory_type <- dynutils::classify_milestone_network(dataset$milestone_network)$network_type
  # dataset$prior_information <- dynwrap::generate_prior_information(dataset$milestone_ids, dataset$milestone_network, dataset$progressions, dataset$milestone_percentages, dataset$counts, dataset$feature_info, dataset$cell_info)
  # dataset$progressions <- with(dataset, dynutils::convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages))

  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
}
