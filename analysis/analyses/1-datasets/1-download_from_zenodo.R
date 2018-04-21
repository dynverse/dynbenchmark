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
task_ids <- list.files(derived_file(""), pattern = ".rds", recursive = TRUE, full.names = FALSE) %>% str_replace(".rds$", "")
pbapply::pblapply(task_ids, function(task_id) {
  file <- derived_file(glue::glue("{task_id}.rds"))

  task <- read_rds(file) %>%
    add_class(paste0("dynwrap::", c("data_wrapper", "with_expression", "with_prior", "with_trajectory"))) %>%
    dynwrap::add_cell_waypoints_to_wrapper() %>%
    .[names(.) != "milenet_spr"]

  task$date <- as.Date(task$date, origin = "1970-01-01")
  task$creation_date = as.POSIXct(task$creation_date, origin = "1970-01-01")

  save_dataset(task, task_id)
  file.remove(file)

  invisible()
})

# pbapply::pblapply(task_ids, function(task_id) {
#   task <- load_dataset(task_id)
#   task$date <- as.Date(task$date, origin = "1970-01-01")
#   task$creation_date = as.POSIXct(task$creation_date, origin = "1970-01-01")
#   save_dataset(task, task_id)
#   invisible()
# })

# make one big tasks tibble, with count and expression as functions.
tasks <- list_as_tibble(map(task_ids, function(task_id) {
  task <- load_dataset(task_id)
  task_file <- dataset_file(filename = "dataset.rds", dataset_id = task_id)
  for (col in c("expression", "counts")) {
    env <- new.env(baseenv())
    assign("task_file", task_file, env)
    assign("col", col, env)
    task[[col]] <- function() {
      readr::read_rds(dynalysis::derived_file(task_file, "1-datasets"))[[col]]
    }
    environment(task[[col]]) <- env
  }
  task
}))
write_rds(tasks, derived_file("tasks.rds"))

# check the size of tasks, in MB
tasks %>% map_int(~ pryr::object_size(.) %>% as.integer) %>% sort(decreasing = T) %>% {. / 1e6} %>% head(10)

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
