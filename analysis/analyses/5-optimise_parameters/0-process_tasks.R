library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/0-process_tasks")

# get the toy data
toy_tasks <- dyntoy::toy_tasks

# get the synthetic data
synthetic_tasks <- read_rds(dataset_file(filename = "tasks.rds", dataset_id = "synthetic/v6"))
synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$settings %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")

# get the real data
real_names <- list_datasets()
real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble()

# combine tasks
tasks <- bind_rows(
  toy_tasks,
  synthetic_tasks,
  real_tasks
)

# compute waypoints for tasks
tasks <-
  seq_len(nrow(tasks)) %>% map(function(i) {
    tasks %>% dynutils::extract_row_to_list(i) %>% dynwrap::add_cell_waypoints_to_wrapper(num_cells_selected = 100)
  }) %>%
  dynutils::list_as_tibble()

tasks %>% mutate(num_cells = map_int(cell_ids, length)) %>% .$num_cells

tasks <- tasks %>%
  rowwise() %>%
  mutate(milenet_spr = milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>% list()) %>%
  ungroup()

# write_rds(tasks, derived_file("tasks.rds"))

sync_tasks(
  tasks = tasks,
  local_tasks_folder = derived_file("tasks"),
  remote_tasks_folder = "/scratch/irc/shared/dynverse_derived/5-optimise_parameters/0-process_tasks/tasks"
)
