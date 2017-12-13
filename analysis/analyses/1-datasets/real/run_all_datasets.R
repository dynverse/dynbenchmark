# Local
dataset_scripts <- list.files(path = "analysis/analyses/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)

for (scr in dataset_scripts) {
  do.call(source, list(scr))
}


## Need both GEOquery and MultiAssayExperiment remotely
library(PRISM)
qsub_conf <- override_qsub_config(memory = "30G", wait=FALSE, r_module = NULL, execute_before="module unload R", stop_on_error=FALSE)

handle <- qsub_lapply(dataset_scripts, function(dataset_script) {
  source(paste0(dynalysis::get_dynalysis_folder(), "/", dataset_script))
  TRUE
}, qsub_environment = list2env(list()), qsub_config = qsub_conf)
saveRDS(handle, "handle.rds")
results <- qsub_retrieve(handle)
