## Estimation of the platforms from real data

library(tidyverse)
library(dynbenchmark)
library(qsub)

experiment("01-platforms")

# remove all platforms
rm_remote(derived_file("", remote = TRUE), remote = TRUE, recursive = TRUE)

# use all real dataset for platform estimation
dataset_ids <- list_datasets() %>% filter(dataset_source == "real") %>% pull(dataset_id)

# run on cluster
qsub_config <- override_qsub_config(
  name = "platform_estim",
  memory = "10G",
  wait = FALSE
)

handle <- qsub_lapply(
  dataset_ids,
  qsub_config = qsub_config,
  estimate_platform
)

write_rds(handle, derived_file("handle.rds"))
handle <- read_rds(derived_file("handle.rds"))

qsub_retrieve(handle)

# sync back locally
qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE
)
