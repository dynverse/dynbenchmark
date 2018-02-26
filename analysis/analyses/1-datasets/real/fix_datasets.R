library(dynalysis)
library(tidyverse)
library(dynplot)

dataset_ids <- list_datasets()

for (i in seq_along(dataset_ids)) {
  dataset_id <- dataset_ids[[i]]
  cat(i, "/", length(dataset_ids), ": ", dataset_id, "\n", sep = "")
  dataset <- load_dataset(dataset_id)

  # update dataset to new wrappers
  # dataset <- with(
  #   dataset,
  #   {
  #     part1 <- progressions %>%
  #       group_by(cell_id) %>%
  #       filter(n() > 1) %>%
  #       ungroup() %>%
  #       select(from, to) %>%
  #       distinct()
  #
  #     if (nrow(part1) > 0) {
  #       divergence_regions <-
  #         part1 %>%
  #         group_by(from) %>%
  #         do({
  #           data_frame(
  #             divergence_id = paste0("div_", .$from[[1]]),
  #             milestone_id = c(.$from[[1]], .$to),
  #             is_start = c(T, rep(F, nrow(.)))
  #           )
  #         }) %>%
  #         ungroup() %>%
  #         select(divergence_id, milestone_id, is_start)
  #     } else {
  #       divergence_regions <- NULL
  #     }
  #
  #     wrap_data(
  #       id = id,
  #       cell_ids = cell_ids,
  #       cell_info = cell_info,
  #       task_source = "real",
  #       cell_grouping = cell_grouping,
  #       normalisation_info = normalisation_info,
  #       creation_date = creation_date
  #     ) %>% add_trajectory_to_wrapper(
  #       milestone_ids = milestone_ids,
  #       milestone_network = milestone_network,
  #       divergence_regions = divergence_regions,
  #       progressions = progressions
  #     ) %>% add_expression_to_wrapper(
  #       counts = counts,
  #       expression = expression,
  #       feature_info = feature_info
  #     ) %>% dynnormaliser::add_prior_information_to_wrapper()
  #   })

  dataset$trajectory_type <- dynutils::classify_milestone_network(dataset$milestone_network)$network_type
  # dataset$geodesic_dist <- dynutils::compute_tented_geodesic_distances(dataset)
  # dataset$prior_information <- dynnormaliser::generate_prior_information(dataset$milestone_ids, dataset$milestone_network, dataset$progressions, dataset$milestone_percentages, dataset$counts, dataset$feature_info, dataset$cell_info)
  # dataset$progressions <- with(dataset, dynutils::convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages))

  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
}
