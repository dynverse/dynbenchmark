# This script preprocesses all datasets, either on the cluster (remote <- TRUE) or locally (remote <- FALSE)

library(dynverse)
library(purrr)
library(readr)

experiment("1-datasets/real/run_all_datasets")

dataset_scripts <- list.files(path = "scripts/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)

remote <- FALSE

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
    X = script_contents[1],
    qsub_environment = character(),
    qsub_config = qsub::override_qsub_config(
      name = "dynreal",
      memory = "30G",
      wait = FALSE,
      execute_before = "",
      stop_on_error = FALSE
    ),
    qsub_packages = c("GEOquery", "MultiAssayExperiment", "tidyverse", "dynbenchmark"),
    FUN = function(script_content) {
      script_content

      dataset_script <- tempfile()
      write_file(script_content, dataset_script)

      source(dataset_script)

      TRUE
    }
  )

  qsub::qsub_retrieve(handle)
}
