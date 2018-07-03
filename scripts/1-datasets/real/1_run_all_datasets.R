# This script preprocesses all datasets, either on the cluster (remote <- TRUE) or locally (remote <- FALSE)

library(purrr)
library(readr)
library(dynbenchmark)

experiment("1-datasets/real/run_all_datasets")

dataset_scripts <- list.files(path = "scripts/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)
# dataset_scripts <- results$dataset_script

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
    X = script_contents,
    qsub_environment = character(),
    qsub_config = qsub::override_qsub_config(
      name = "dynreal",
      memory = "30G",
      wait = FALSE,
      execute_before = "",
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

  results <- qsub::qsub_retrieve(handle)

  names(results) <- dataset_scripts

  results <- results %>%
    keep(is.na) %>%
    enframe("dataset_script", "result") %>%
    mutate(error = map_chr(result, ~attr(., "qsub_error")))
}



datasets <- load_datasets(list_datasets() %>% filter(dataset_source == "real") %>% pull(dataset_id))

dataset <- extract_row_to_list(datasets, 8)
dataset %>% dynplot::plot_dimred(grouping = dataset$grouping, dimred = dyndimred::dimred_mds, plot_milestone_network = T)


# sync back locally
qsub::rsync_remote()





