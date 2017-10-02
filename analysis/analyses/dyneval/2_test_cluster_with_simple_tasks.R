library(dyneval)
library(dyntoy)
library(tidyverse)

# specify folders
derived_dir <- "analysis/data/derived_data/dyneval/2_test_cluster_with_simple_tasks/"
dyngen_derived_dir <- "analysis/data/derived_data/dyngen/"
.datasets_location <- paste0(dyngen_derived_dir, "4/")

# create derived output dir
dir.create(derived_dir, recursive = T)

# preprocess tasks
tasks <- load_datasets(8) # this function takes way too long due to the geodesic distances being calculated
saveRDS(tasks, paste0(dyngen_derived_dir, "tasks_v4.rds"))

# load tasks
tasks <- readRDS(paste0(dyngen_derived_dir, "tasks_v4.rds")) %>%
  filter(
    platform_id == "fluidigm_c1",
    takesetting_type == "snapshot",
    ti_type == "consecutive_bifurcating",
    model_replicate %in% c(1,2))

# configure run
task_group <- rep("group", nrow(tasks))
task_fold <- tasks$model_replicate
methods <- get_descriptions(as_tibble = T)

# submit benchmarks
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_dir,
  methods = methods,
  metrics = c("correlation", "robbie_network_score"),
  timeout = 600,
  memory = "16G",
  num_cores = 1,
  num_iterations = 10,
  num_init_params = 20,
  save_r2g_to_outdir = TRUE
)

# retrieve results
benchmark_suite_retrieve(derived_dir)
