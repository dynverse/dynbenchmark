library(dynalysis)
library(PRISM)

experiment("1-datasets/real/run_all_datasets")

# # Local
dataset_scripts <- list.files(path = "analysis/analyses/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)
#
# for (scr in dataset_scripts) {
#   do.call(source, list(scr))
# }

## Need both GEOquery and MultiAssayExperiment installed on PRISM
handle <- qsub_lapply(
  X = dataset_scripts,
  qsub_environment = list2env(list()),
  qsub_config = override_qsub_config(
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
    oldwd <- getwd()
    setwd("/group/irc/shared/dynalysis/")
    source(paste0(dynalysis::get_dynalysis_folder(), "/", dataset_script))
    setwd(oldwd)
    TRUE
  }
)

write_rds(handle, derived_file("handle.rds"))

results <- qsub_retrieve(read_rds(derived_file("handle.rds")))
