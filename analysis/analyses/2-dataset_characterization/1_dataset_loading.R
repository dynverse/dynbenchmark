library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("dataset_characterisation")

map_prism <- function (x, f) PRISM::qsub_lapply(x, f, qsub_environment = list2env(list()), qsub_config = PRISM::override_qsub_config(memory="4G"))
map_local <- function(x, f) pbapply::pblapply(x, f, cl=1)
map_local_parallel <- function(x, f) pbapply::pblapply(x, f, cl=8)

# load in info of real tasks from google spreadsheet
task_info_real <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",\n") %>%
  mutate(id = paste0("real/", id)) %>%
  mutate(category = "real")

task_ids_real <- task_info_real$id

# tasks not online but found physically
paste0("real/", list.files("analysis/data/derived_data/datasets/real/"))[!(paste0("real/", list.files("analysis/data/derived_data/datasets/real/")) %in% task_ids_real)]

task_ids_real[!(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]
task_ids_real <- task_ids_real[(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]

tasks_synthetic <- read_rds("analysis/data/derived_data/datasets/synthetic/v6/tasks.rds")
tasks_synthetic <- tasks_synthetic %>% mutate(category = "synthetic")

# create functions to dynamically load in expression and count data of real tasks
tasks_real <- map_local(task_ids_real, function(task_id) {
  print(task_id)
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  print(task$trajectory_type)

  task$expression <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$expression}
  task$counts <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$counts}

  dynutils::list_as_tibble(list(task))
}) %>% bind_rows()
tasks_real <- tasks_real %>% left_join(task_info_real, "id")

tasks_toy <- dyntoy::toy_tasks

tasks <- bind_rows(tasks_synthetic, tasks_real, toy_tasks)

tasks$category <- ifelse(tasks$id %>% startsWith("real"), "real", ifelse(tasks$id %>% startsWith("toy"), "toy", "synthetic"))

saveRDS(tasks, derived_file("tasks.rds"))
