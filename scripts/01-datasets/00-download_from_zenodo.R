#' Downloading the processed datasets from Zenodo ([10.5281/zenodo.1211532](https://doi.org/10.5281/zenodo.1211532))

library(tidyverse)
library(dynbenchmark)

experiment("01-datasets")

# download the zip file from zenodo
dataset_file <- derived_file("datasets.zip")
if (!file.exists(dataset_file)) download.file("https://zenodo.org/record/1211533/files/datasets.zip", dataset_file)

# unzip the folder
unzip(dataset_file, exdir = derived_file(""))

# remove zip
file.remove(dataset_file)

# temporary fixes;
# - the classes were accidentally removed from each dataset before submission to zenodo
# - add divergence regions
# - add cell waypoints
# - process dates
dataset_ids <- list.files(derived_file(""), pattern = ".rds", recursive = TRUE, full.names = FALSE) %>%
  str_replace(".rds$", "") %>%
  discard(~. == "datasets")

pbapply::pblapply(dataset_ids, function(dataset_id) {
  file <- derived_file(glue::glue("{dataset_id}.rds"))

  dataset <- read_rds(file) %>%
    add_class(paste0("dynwrap::", c("data_wrapper", "with_expression", "with_prior", "with_trajectory")))

  dataset$divergence_regions <- tibble(milestone_id = character(), divergence_id = character(), is_start = logical())

  dataset <- dynwrap::add_cell_waypoints(dataset)

  names(dataset) <- str_replace(names(dataset), "^task_", "dataset_")
  dataset$milenet_spr <- NULL
  dataset$.object_class <- NULL
  dataset$date <- as.Date(dataset$date, origin = "1970-01-01")
  dataset$creation_date = as.POSIXct(dataset$creation_date, origin = "1970-01-01")

  save_dataset(dataset, dataset_id)
  file.remove(file)

  invisible()
})

# upload to remote, if relevant
# this assumes you have a gridengine cluster at hand
qsub::rsync_remote(
  remote_src = FALSE,
  path_src = derived_file(),
  remote_dest = TRUE,
  path_dest = derived_file(remote = TRUE)
)
