library(tidyverse)
library(dynalysis)

experiment(
  dirname = "dyngen",
  description = "Testing whether each method is able to run on the cluster with a real task",
  auto_create_folders = TRUE
)

# update the local dyngen datasets from what is stored on the prism
PRISM:::rsync_remote(
  remote_src = "prism",
  path_src = "/group/irc/shared/dynalysis/analysis/data/",
  remote_dest = "",
  path_dest = paste0(getwd(), "/analysis/data/")
)

# load tasks from files
.datasets_location <- scratch_file("4/")
tasks <- dyngen::load_datasets()

saveRDS(tasks, scratch_file("tasks_v4.rds"))
