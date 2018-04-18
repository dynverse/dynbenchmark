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
  traj <- read_rds(file) %>%
    add_class(paste0("dynwrap::", c("data_wrapper", "with_expression", "with_prior", "with_trajectory"))) %>%
    dynwrap::add_cell_waypoints_to_wrapper() %>%
    .[names(.) != "milenet_spr"]
  write_rds(traj, file)
}

# load all tasks and perform visual check
tasks <- list_as_tibble(map(traj_files, read_rds))
tasks

# upload to remote, if relevant
# this assumes you have a gridengine cluster at hand
remote_config <- PRISM::override_qsub_config()
remote_dynalysis_path <- PRISM:::run_remote("echo $DYNALYSIS_PATH", remote = remote_config$remote)$cmd_out
PRISM:::rsync_remote(
  remote_src = "",
  path_src = derived_file(),
  remote_dest = remote_config$remote,
  path_dest = derived_file() %>% str_replace(get_dynalysis_folder(), remote_dynalysis_path)
)
