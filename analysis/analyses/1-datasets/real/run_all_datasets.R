
dataset_scripts <- list.files(path = "analysis/analyses/1-datasets/real", pattern = "^dataset_.*\\.R", full.names = TRUE)

for (scr in dataset_scripts) {
  do.call(source, scr)
}
