library(dynbenchmark)
library(tidyverse)
library(qsub)

# determine datasets to normalise & filter
list_raw_datasets_remote <- function() {
  derived_file(experiment_id = "1-datasets_preproc/raw/real", remote = T) %>%
    ls_remote(remote = T) %>%
    paste0("real/", .) %>%
    gsub("(.*)\\.rds", "\\1", .)
}

list_datasets_remote <- function() {
  derived_file(experiment_id = "1-datasets/real", remote = T) %>%
    ls_remote(remote = T) %>%
    paste0("real/", .) %>%
    gsub("(.*)\\.rds", "\\1", .)
}

dataset_ids_to_process <- setdiff(list_raw_datasets_remote(), list_datasets_remote())

process_raw_dataset_wrapper <- function(dataset_id) {
  raw_dataset <- readr::read_rds(dynbenchmark::dataset_raw_file(dataset_id))
  raw_dataset$id <- dataset_id

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


dataset_id <- "real/dentate-gyrus-neurogenesis_hochgerner"
process_raw_dataset_wrapper(dataset_id)
dataset <- load_dataset(dataset_id)

dynplot::plot_dimred(dataset)















remote_dynbenchmark_folder <- qsub::qsub_lapply(1, function(x) dynbenchmark::get_dynbenchmark_folder(), qsub_environment = character())[[1]]
local_dynbenchmark_folder <- dynbenchmark::get_dynbenchmark_folder()
local_folder_sync <- dynbenchmark::derived_file("", "1-datasets/real/")
remote_folder_sync <- gsub(local_dynbenchmark_folder, remote_dynbenchmark_folder, local_folder_sync)

qsub::rsync_remote("prism", remote_folder_sync, "", local_folder_sync)
