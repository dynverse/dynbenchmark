#' Downloading the real datasets from their sources (eg. GEO), and constructing the gold standard model, using the helpers in [helpers-download_from_sources](helpers-download_from_sources)

library(tidyverse)
library(dynbenchmark)

experiment("01-datasets/real/run_all_datasets")

dataset_scripts <- list.files(path = "scripts/01-datasets/01-real/helpers-download_from_sources/", pattern = "^dataset_.*\\.R", full.names = TRUE)
# dataset_scripts <- dataset_scripts[str_detect(dataset_scripts, "mouse-cell-atlas") | str_detect(dataset_scripts, "plass")]

script_contents <- map_chr(dataset_scripts, read_file)

qsub_config <- qsub::override_qsub_config(
  name = "dynreal",
  memory = "30G",
  max_wall_time = "02:00:00",
  wait = FALSE,
  stop_on_error = FALSE
)

# Make sure all packages are installed on the cluster; i.e. GEOquery, MultiAssayExperiment, tidyverse, and dynbenchmark.
handle <- qsub::qsub_lapply(
  script_contents,
  function(script_content) {
    dataset_script <- tempfile()
    readr::write_file(script_content, dataset_script)

    source(dataset_script)

    TRUE
  },
  qsub_environment = new.env(),
  qsub_config = qsub_config
)

write_rds(handle, "handle.rds")

##

handle <- read_rds("handle.rds")
results <- qsub::qsub_retrieve(handle, wait = TRUE)

names(results) <- dataset_scripts

results <- results %>%
  keep(is.na) %>%
  enframe("dataset_script", "result")
