library(dynalysis)

experiment("1-datasets/real/run_all_datasets")

dataset_scripts <- list.files(path = "analysis/analyses/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)

remote <- FALSE

if (remote) {
  ## Local
  for (scr in dataset_scripts) {
    do.call(source, list(scr))
  }
} else {
  ## Remote
  # Make sure all packages are installed on the cluster; i.e. GEOquery, MultiAssayExperiment, tidyverse, and dynalysis.
  handle <- qsub::qsub_lapply(
    X = dataset_scripts,
    qsub_environment = list2env(list()),
    qsub_config = qsub::override_qsub_config(
      name = "dynreal",
      memory = "30G",
      wait = FALSE,
      r_module = NULL,
      execute_before = "",
      stop_on_error = FALSE,
      remove_tmp_folder = FALSE
    ),
    qsub_packages = c("GEOquery", "MultiAssayExperiment", "tidyverse", "dynalysis"),
    FUN = function(dataset_script) {
      cat("Running ", sQuote(dataset_script), "\n", sep = "")
      oldwd <- getwd()
      setwd("/group/irc/shared/dynalysis/")
      source(paste0(dynalysis::get_dynalysis_folder(), "/", dataset_script))
      setwd(oldwd)
      TRUE
    }
  )
}
