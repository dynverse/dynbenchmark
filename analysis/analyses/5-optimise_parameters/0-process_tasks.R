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
  toy_tasks %>% mutate(task_group = "toy"),
  synthetic_tasks %>% mutate(task_group = "synthetic"),
  real_tasks %>% mutate(task_group = "real")
)

selected_colnames <- Reduce("intersect", list(colnames(toy_tasks), colnames(synthetic_tasks), colnames(real_tasks)))
tasks <- tasks %>% select(one_of(c("task_group", selected_colnames)))
write_rds(tasks, derived_file("tasks.rds"))
