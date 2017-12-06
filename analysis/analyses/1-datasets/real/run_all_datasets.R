
dataset_scripts <- list.files(path = "analysis/analyses/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)

for (scr in dataset_scripts) {
  do.call(source, list(scr))
}









library(PRISM)
qsub_conf <- override_qsub_config(memory = "30G", wait=FALSE, r_module = NULL, execute_before="module unload R")

handle <- qsub_lapply(dataset_scripts, function(dataset_script) {
  wd <- getwd()
  setwd(dynalysis::get_dynalysis_folder())
  source(dataset_script)
  setwd(wd)
}, qsub_environment = list2env(list()), qsub_config = qsub_conf)
qsub_retrieve(handle)
