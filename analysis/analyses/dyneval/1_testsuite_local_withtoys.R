library(dynalysis)
library(tidyverse)
library(mlr)
library(mlrMBO)

experiment("dyneval/1_testsuite_local_withtoys")

# trying all methods
methods <- dyneval::get_descriptions(T) %>% filter(name == "SCORPIUS")

# toys
tasks <- dyntoy::toy_tasks[c(2,5),]
task_group <- rep("group", nrow(tasks))
task_fold <- tasks$replicate

#metrics <- c("auc_R_nx", "correlation")
metrics <- c("auc_R_nx")
timeout <- 600

# start benchmark suite
out <- benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_file("suite/"),
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = timeout,
  memory = NA,
  num_cores = 8,
  num_iterations = 10,
  num_repeats = 1,
  num_init_params = 99,
  do_it_local = TRUE
)

plot(out[[1]][[1]]$tune_train)
