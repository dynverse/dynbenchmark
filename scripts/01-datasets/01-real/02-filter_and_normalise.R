#' Filtering and normalising the real datasets using `dynbenchmark::process_raw_dataset` All datasets are then saved into the dynwrap format.

library(dynbenchmark)
library(tidyverse)
library(qsub)

run_remote("find . -maxdepth 2 -type d -ls", args = c("-2", derived_file(experiment_id = '01-datasets_preproc/raw/real', remote = T)), remote = T)$stdout

# determine datasets to normalise & filter
list_raw_datasets_remote <- function() {
  run_remote(
    "find",
    args = c(
      derived_file(experiment_id = '01-datasets_preproc/raw/real', remote = T),
      "-maxdepth", "2",
      "-type", "f"
    ),
    remote = T
  )$stdout %>%
    gsub(".*(real/.*)\\.rds", "\\1", .)
}

list_datasets_remote <- function() {
  run_remote(
    "find",
    args = c(
      derived_file(experiment_id = '01-datasets/real', remote = T),
      "-maxdepth", "3",
      "-type", "f",
      "-regex", "'.*dataset\\.rds'"
    ),
    remote = T
  )$stdout %>%
    gsub(".*(real/.*)/dataset\\.rds", "\\1", .)
}

dataset_ids_to_process <- setdiff(list_raw_datasets_remote(), list_datasets_remote())

process_raw_dataset_wrapper <- function(id) {
  raw_dataset <- readr::read_rds(dynbenchmark::dataset_raw_file(id))
  raw_dataset$id <- id

  do.call(dynbenchmark::process_raw_dataset, raw_dataset)
  TRUE
}

qsub_config <- qsub::override_qsub_config(
  name = "filter_norm",
  memory = "30G",
  wait = FALSE,
  stop_on_error = TRUE
)

handle <- qsub_lapply(dataset_ids_to_process, process_raw_dataset_wrapper, qsub_environment = character(), qsub_config = qsub_config)


# sync locally
local_folder_sync <- derived_file("", "01-datasets/real/")
remote_folder_sync <- derived_file("", "01-datasets/real/", remote = TRUE)

qsub::rsync_remote("prism", remote_folder_sync, "", local_folder_sync)
