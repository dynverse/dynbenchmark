library(tidyverse)
library(dynbenchmark)

experiment("01-datasets")

# download the zip file from zenodo
dataset_file <- derived_file("datasets.zip")
if (!file.exists(dataset_file)) download.file("https://zenodo.org/record/1211533/files/datasets.zip", dataset_file)

# unzip the folder
unzip(dataset_file, exdir = derived_file(""))

# remove zip
# file.remove(dataset_file)

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

  dataset$milenet_spr <- NULL
  dataset$.object_class <- NULL
  dataset$date <- as.Date(dataset$date, origin = "1970-01-01")
  dataset$creation_date = as.POSIXct(dataset$creation_date, origin = "1970-01-01")

  save_dataset(dataset, dataset_id)
  file.remove(file)

  invisible()
})

# make one big datasets tibble, with count and expression as functions.
datasets <- list_as_tibble(map(dataset_ids, function(dataset_id) {
  dataset <- load_dataset(dataset_id) %>% extract_row_to_list(1)
  dataset_file <- dataset_file(filename = "dataset.rds", dataset_id = dataset_id)
  for (col in c("expression", "counts")) {
    env <- new.env(baseenv())
    assign("dataset_id", dataset_id, env)
    assign("col", col, env)
    dataset[[col]] <- function() {
      dynbenchmark::load_dataset(dataset_id)[[col]]
    }
    environment(dataset[[col]]) <- env
  }

  dataset
}))

# todo: list_as_tibble needs to handle dates correctly
datasets$date <- as.Date(datasets$date, origin = "1970-01-01")
datasets$creation_date = as.POSIXct(datasets$creation_date, origin = "1970-01-01")
write_rds(datasets, derived_file("datasets.rds"))

# check the size of datasets, in MB
datasets %>% map_int(~ pryr::object_size(.) %>% as.integer) %>% sort(decreasing = T) %>% {. / 1e6} %>% head(10)

# upload to remote, if relevant
# this assumes you have a gridengine cluster at hand
remote_config <- qsub::override_qsub_config()
remote_dynbenchmark_path <- qsub:::run_remote("echo $DYNBENCHMARK_PATH", remote = remote_config$remote)$stdout
qsub:::rsync_remote(
  remote_src = "",
  path_src = derived_file(),
  remote_dest = remote_config$remote,
  path_dest = derived_file() %>% str_replace(get_dynbenchmark_folder(), remote_dynbenchmark_path)
)
