#' Download the datasets from the cluster

library(dynbenchmark)
library(tidyverse)

experiment("01-datasets")

qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE,
  compress = FALSE
)

# Upload to prism
# qsub::rsync_remote(
#   remote_src = FALSE,
#   path_src = derived_file(remote = FALSE),
#   remote_dest = TRUE,
#   path_dest = derived_file(remote = TRUE),
#   verbose = TRUE
# )
