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
  mutate(category = "real")

task_ids_real <- task_info_real$id

# tasks not online but found physically
paste0("real/", list.files("analysis/data/derived_data/datasets/real/"))[!(paste0("real/", list.files("analysis/data/derived_data/datasets/real/")) %in% task_ids_real)]

task_ids_real[!(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]
task_ids_real <- task_ids_real[(task_ids_real %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]

# create functions to dynamically load in expression and count data of real tasks
tasks_real <- map_local(task_ids_real, function(task_id) {
  print(task_id)
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  print(task$trajectory_type)

  task$expression <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$expression}
  task$counts <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$counts}

  dynutils::list_as_tibble(list(task))
}) %>% bind_rows()
tasks_real <- tasks_real %>% left_join(task_info_real, "id") %>% mutate(category = "real") %>% mutate(task_id = id)

##  ............................................................................
##  Synthetic datasets                                                      ####
tasks_synthetic <- read_rds("analysis/data/derived_data/datasets/synthetic/v6/tasks.rds") %>%
  rename(task_id = id) %>%
  mutate(id = paste0("synthetic/", task_id)) %>%
  mutate(category = "synthetic")

##  ............................................................................
##  Toy datasets                                                            ####
tasks_toy <- dyntoy::toy_tasks %>%
  mutate(category = "toy") %>%
  rename(task_id = id) %>%
  mutate(id = paste0("toy/", task_id))


##  ............................................................................
##  Control datasets                                                        ####
tasks_control <- read_rds(derived_file("tasks.rds", "1-datasets/control")) %>%
  rename(task_id = id) %>%
  mutate(id = paste0("control/", task_id)) %>%
  mutate(category = "control")


##  ............................................................................
##  Combine datasets                                                        ####
tasks <- bind_rows(tasks_synthetic, tasks_real, tasks_toy, tasks_control)

saveRDS(tasks, derived_file("tasks.rds"))


#   ____________________________________________________________________________
#   Check datasets                                                          ####

load_expression <- function(expression) {
  if(class(expression) == "matrix") {
    expression
  } else {
    expression()
  }
}

task_checks <- pbapply::pblapply(tasks$id, function(task_id) {
  print(task_id)
  task <- extract_row_to_list(tasks, which(tasks$id == task_id))
  expression <- load_expression(task$expression)
  counts <- load_expression(task$counts)

  checks <- list(id = task_id)

  checks$all_milestones_represented <- all(task$milestone_ids %in% task$prior_information$grouping_assignment$group_id)

  checks$marker_feature_ids_in_expression <- all(task$prior_information$marker_feature_ids %in% colnames(expression))

  checks$columns <- all(c("geodesic_dist") %in% names(task))

  checks$n_genes <- ncol(counts)
  checks$n_cells <- nrow(counts)

  checks$task_id <- task$id

  checks
}) %>% bind_rows()

task_checks %>% bind_cols(tasks) %>% ggplot() + geom_point(aes(n_genes, n_cells, color=category))

task_ids_filtered <- task_checks %>% filter(all_milestones_represented) %>% pull(task_id)
tasks_filtered <- tasks %>% filter(id %in% task_ids_filtered)
