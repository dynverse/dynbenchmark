library(dyneval)
library(dyntoy)
library(tidyverse)

derived_dir <- "analysis/data/derived_data/dyneval/1_test_cluster_with_toys/"
dir.create(derived_dir, recursive = T)

# easy test
tasks <- generate_toy_datasets(num_replicates = 2)
task_group <- rep("group", nrow(tasks))
task_fold <- gsub(".*_", "", tasks$id) %>% as.integer()
methods <- get_descriptions(as_tibble = T)

benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_dir,
  methods = methods,
  metrics = c("correlation", "robbie_network_score"),
  timeout = 600,
  memory = "16G",
  num_cores = 2,
  num_iterations = 5,
  num_init_params = 16
)

benchmark_suite_retrieve(derived_dir)
