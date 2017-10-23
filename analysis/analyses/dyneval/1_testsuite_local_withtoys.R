library(dynalysis)
library(tidyverse)
library(mlr)
library(mlrMBO)

experiment(
  dirname = "dyneval/1_testsuite_local_withtoys",
  description = "Testing whether each method is able to run locally",
  auto_create_folders = TRUE
)

# trying all methods
methods <- dyneval::get_descriptions(T) %>% filter(name == "shuffle")

# toys
tasks <- dyntoy::toy_tasks[c(2,5),]
task_group <- rep("group", nrow(tasks))
task_fold <- gsub(".*_([0-9]*)$", "\\1", tasks$id) %>% as.integer

#metrics <- c("auc_R_nx", "correlation")
metrics <- c("auc_R_nx")
timeout <- 60

# start benchmark suite
out <- benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = scratch_file("suite/"),
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = timeout,
  memory = NA,
  num_cores = 1,
  num_iterations = 5,
  num_repeats = 1,
  num_init_params = 16,
  do_it_local = TRUE
)
