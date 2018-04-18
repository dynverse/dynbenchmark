library(tidyverse)
library(dynalysis)

experiment("1-datasets")

# download the zip file from zenodo
dataset_file <- derived_file("datasets.zip")
download.file("https://zenodo.org/record/1211533/files/datasets.zip", dataset_file)

# unzip the folder
unzip(dataset_file, exdir = derived_file(""))

# remove zip
file.remove(dataset_file)

# temporary fix; the classes were accidentally removed from each dataset before submission to zenodo
traj_files <- list.files(derived_file(""), pattern = ".rds", recursive = TRUE, full.names = TRUE)
for (file in traj_files) {
  traj <- read_rds(file)
  class(traj) <- c(paste0("dynwrap::", c("data_wrapper", "with_expression", "with_prior", "with_trajectory")), "list")
  write_rds(traj, file)
}
