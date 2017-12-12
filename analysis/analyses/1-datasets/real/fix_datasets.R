library(dynalysis)
library(tidyverse)
library(dynplot)

dataset_ids <- list_datasets()

for (i in seq_along(dataset_ids)) {
  dataset_id <- dataset_ids[[i]]
  cat(i, "/", length(dataset_ids), ": ", dataset_id, "\n", sep = "")
  dataset <- load_dataset(dataset_id)
  dataset$trajectory_type <- dynutils::classify_milestone_network(dataset$milestone_network)$network_type
  dataset$geodesic_dist <- dynutils::compute_emlike_dist(dataset)

  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
}
