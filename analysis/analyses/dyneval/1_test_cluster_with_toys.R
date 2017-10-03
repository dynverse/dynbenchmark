library(dyneval)
library(dyntoy)
library(dynutils)
library(tidyverse)

derived_dir <- "analysis/data/derived_data/dyneval/1_test_cluster_with_toys/"

# # remove previous output
# unlink(derived_dir, recursive=TRUE)

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

outputs <- benchmark_suite_retrieve(derived_dir)

succeeded <- outputs %>% list_as_tibble() %>% filter(!sapply(which_errored, any))

all_outputs <- succeeded$outputs %>% map_df(function(x) x %>% map_df(~ .$eval_grp))
all_outputs %>% select(ti_type, grid_i, param_i, fold_type, method_name, correlation, robbie_network_score)
ggplot(all_outputs) + geom_point(aes())
