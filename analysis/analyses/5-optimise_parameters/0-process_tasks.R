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

tasks <- tasks %>%
  rowwise() %>%
  mutate(milenet_spr = milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>% list()) %>%
  ungroup()

write_rds(tasks, derived_file("tasks.rds"))
