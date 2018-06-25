library(tidyverse)
library(dynalysis)

remdyn <- "/group/irc/shared/dynalysis"

# Download from PRISM ---------------------
qsub:::rsync_remote(
  remote_src = "prism",
  path_src = paste0(remdyn, dataset_file("", "real", relative = T)),
  remote_dest = "",
  path_dest = dataset_file("", "real")
)
qsub:::rsync_remote(
  remote_src = "prism",
  path_src = paste0(remdyn, dataset_file("", "synthetic", relative = T)),
  remote_dest = "",
  path_dest = dataset_file("", "synthetic")
)
qsub:::rsync_remote(
  remote_src = "prism",
  path_src = paste0(remdyn, dataset_file("", "control", relative = T)),
  remote_dest = "",
  path_dest = dataset_file("", "control")
)


# Upload to PRISM ---------------------
qsub:::rsync_remote(
  remote_dest = "prism",
  path_dest = paste0(remdyn, dataset_file("", "real", relative = T)),
  remote_src = "",
  path_src = dataset_file("", "real")
)
qsub:::rsync_remote(
  remote_dest = "prism",
  path_dest = paste0(remdyn, dataset_file("", "synthetic", relative = T)),
  remote_src = "",
  path_src = dataset_file("", "synthetic")
)
qsub:::rsync_remote(
  remote_dest = "prism",
  path_dest = paste0(remdyn, dataset_file("", "control", relative = T)),
  remote_src = "",
  path_src = dataset_file("", "control")
)
