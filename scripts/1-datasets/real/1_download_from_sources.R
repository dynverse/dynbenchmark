# This script preprocesses all datasets, either on the cluster (remote <- TRUE) or locally (remote <- FALSE)

library(tidyverse)
library(dynbenchmark)

experiment("1-datasets/real/run_all_datasets")

dataset_scripts <- list.files(path = "scripts/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)
# dataset_scripts <- dataset_scripts[!str_detect(dataset_scripts, "mouse-cell-atlas")]

remote <- TRUE

if (remote) {
  ## Local
  for (scr in dataset_scripts) {
    do.call(source, list(scr))
  }
} else {
  ## Remote
  # Make sure all packages are installed on the cluster; i.e. GEOquery, MultiAssayExperiment, tidyverse, and dynbenchmark.
  script_contents <- map_chr(dataset_scripts, read_file)

  handle <- qsub::qsub_lapply(
    X = script_contents,
    qsub_environment = character(),
    qsub_config = qsub::override_qsub_config(
      name = "dynreal",
      memory = "30G",
      wait = FALSE,
      stop_on_error = FALSE
    ),
    qsub_packages = "dynbenchmark",
    FUN = function(script_content) {
      dataset_script <- tempfile()
      readr::write_file(script_content, dataset_script)

      source(dataset_script)

      TRUE
    }
  )

  write_rds(handle, "handle.rds")

  results <- qsub::qsub_retrieve(handle)

  names(results) <- dataset_scripts

  results <- results %>%
    keep(is.na) %>%
    enframe("dataset_script", "result") %>%
    mutate(error = map_chr(result, ~attr(., "qsub_error")))
}


remote_dynbenchmark_folder <- qsub::qsub_lapply(1, function(x) dynbenchmark::get_dynbenchmark_folder(), qsub_environment = character())[[1]]
local_dynbenchmark_folder <- dynbenchmark::get_dynbenchmark_folder()
local_folder_sync <- dynbenchmark::derived_file("", "1-datasets/real/")
remote_folder_sync <- gsub(local_dynbenchmark_folder, remote_dynbenchmark_folder, local_folder_sync)

qsub::rsync_remote("prism", remote_folder_sync, "", local_folder_sync)
