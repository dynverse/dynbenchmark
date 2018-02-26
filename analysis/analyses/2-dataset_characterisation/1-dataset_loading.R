library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("2-dataset_characterisation")

map_prism <- function (x, f) PRISM::qsub_lapply(x, f, qsub_environment = list2env(list()), qsub_config = PRISM::override_qsub_config(memory="4G"))
map_local <- function(x, f) pbapply::pblapply(x, f, cl=1)
map_local_parallel <- function(x, f) pbapply::pblapply(x, f, cl=8)



#   ____________________________________________________________________________
#   Load datasets                                                           ####

##  ............................................................................
##  Real datasets                                                           ####
task_info_real <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",\n") %>%
  mutate(id = paste0("real/", id)) %>%
  mutate(category = "real") %>%
  filter(ready)

task_ids_real <- task_info_real$id

# tasks not online but found physically
paste0("real/", list.files("analysis/data/derived_data/datasets/real/"))[!(paste0("real/", list.files("analysis/data/derived_data/datasets/real/")) %in% task_ids_real)]

task_ids_real[!(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]
task_ids_real <- task_ids_real[(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]

# create functions to dynamically load in expression and count data of real tasks
tasks_real <- map_local(task_ids_real, function(task_id) {
  print(task_id)
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  env <- new.env(baseenv())
  assign("task_id", task_id, env)

  task$expression <- function() {readRDS(dynalysis::dataset_file("dataset.rds", dataset_id = task_id))$expression}
  task$counts <- function() {readRDS(dynalysis::dataset_file("dataset.rds", dataset_id = task_id))$counts}
  task$geodesic_dist <- function() {readRDS(dynalysis::dataset_file("dataset.rds", dataset_id = task_id))$geodesic_dist}
  environment(task$expression) <- env
  environment(task$counts) <- env
  environment(task$geodesic_dist) <- env

  task %>% list %>% dynutils::list_as_tibble()
}) %>% bind_rows() %>% mutate(task_group = "real")
tasks_real <- tasks_real %>% left_join(task_info_real, "id") %>% mutate(category = "real") %>% mutate(task_id = id)

##  ............................................................................
##  Synthetic datasets                                                      ####
synthetic_folder <- "analysis/data/derived_data/datasets/synthetic/v6/"
tasks_synthetic <- read_rds(paste0(synthetic_folder, "tasks.rds")) %>%
  mutate(task_group = "synthetic")
tasks_synthetic <- map(seq_len(nrow(tasks_synthetic)), function(task_i) {
  task <- dynutils::extract_row_to_list(tasks_synthetic, task_i)

  env <- new.env(baseenv())
  assign("task_id", task$task_id, env)
  assign("synthetic_folder", synthetic_folder, env)

  # to re save
  # task$expression %>% saveRDS(paste0(synthetic_folder, task_i, "_expression.rds"))
  # task$counts %>% saveRDS(paste0(synthetic_folder, task_i, "_counts.rds"))
  # task$geodesic_dist %>% saveRDS(paste0(synthetic_folder, task_i, "_geodesic_dist.rds"))

  task$expression <- function() {readRDS(paste0(synthetic_folder, task_i, "_expression.rds"))}
  task$counts <- function() {readRDS(paste0(synthetic_folder, task_i, "_counts.rds"))}
  task$geodesic_dist <- function() {readRDS(paste0(synthetic_folder, task_i, "_geodesic_dist.rds"))}

  environment(task$expression) <- env
  environment(task$counts) <- env
  environment(task$geodesic_dist) <- env

  task
}) %>% dynutils::list_as_tibble() %>% mutate(id = paste0("synthetic/", id))


##  ............................................................................
##  Toy datasets                                                            ####
tasks_toy <- dyntoy::toy_tasks %>%
  mutate(task_group = "toy")


##  ............................................................................
##  Control datasets                                                        ####
tasks_control <- read_rds(dataset_file("tasks.rds", "control")) %>%
  mutate(task_group = "control")


##  ............................................................................
##  Combine datasets                                                        ####
tasks <- bind_rows(tasks_synthetic, tasks_real, tasks_toy, tasks_control)


##  ............................................................................
##  Add characteristics                                                     ####
task_characteristics <- pbapply::pblapply(tasks_to_check$id, function(task_id) {
  print(task_id)
  task <- extract_row_to_list(tasks, which(tasks$id == task_id))
  counts <- load(task$counts)

  characteristics <- list(id = task_id)

  characteristics$n_genes <- ncol(counts)
  characteristics$n_cells <- nrow(counts)

  characteristics
}) %>% bind_rows()
tasks <- left_join(tasks[c("id", colnames(tasks)[!colnames(tasks) %in% colnames(task_characteristics)])], task_characteristics, by="id")

saveRDS(tasks, derived_file("tasks.rds"))


#   ____________________________________________________________________________
#   Check datasets                                                          ####

load <- function(x) {
  if(class(x) == "matrix") {
    x
  } else {
    x()
  }
}

tasks_to_check <-tasks# %>% filter(id == "real/mESC-differentiation_hayashi")
task_checks <- pbapply::pblapply(tasks_to_check$id, function(task_id) {
  print(task_id)
  task <- extract_row_to_list(tasks, which(tasks$id == task_id))
  expression <- load(task$expression)
  counts <- load(task$counts)

  checks <- list(id = task_id)

  checks$check_all_milestones_represented <- all(task$milestone_ids %in% task$prior_information$grouping_assignment$group_id)

  checks$check_marker_feature_ids_in_expression <- all(task$prior_information$marker_feature_ids %in% colnames(expression))

  checks$check_double_froms <- all(
    (task$progressions %>% group_by(cell_id) %>% summarise(n=length(unique(from))) %>% pull(n)) == 1
  )

  checks$check_columns <- all(c("geodesic_dist") %in% names(task))

  checks$task_id <- task$id

  checks
}) %>% bind_rows()

task_checks %>%
  bind_cols(tasks_to_check) %>%
  ggplot() +
    geom_point(aes(n_genes, n_cells, color=task_group)) +
    scale_x_log10() +
    scale_y_log10()

task_checks %>% select(id, starts_with("check")) %>% gather("check_id", "passed", -id) %>%
  ggplot() +
    geom_raster(aes(check_id, id, fill=passed))

saveRDS(task_checks, derived_file("tasks_checks.rds"))

task_ids_filtered <- task_checks %>% filter(check_all_milestones_represented) %>% pull(task_id)
tasks_filtered <- tasks %>% filter(id %in% task_ids_filtered)
