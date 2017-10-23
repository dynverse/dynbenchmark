library(dynalysis)
library(tidyverse)

experiment(
  dirname = "dyneval/3_testsuite_cluster_withtoys",
  description = "Testing whether each method is able to run on the cluster",
  auto_create_folders = TRUE
)

# remove previous output
# unlink(scratch_file("suite/"), recursive = T, force = T)

# trying all methods
methods <- dyneval::get_descriptions()

# toys
tasks <- dyntoy::toy_tasks[c(2,5),]
task_group <- rep("group", nrow(tasks))
task_fold <- gsub(".*_([0-9]*)$", "\\1", tasks$id) %>% as.integer

#metrics <- c("auc_R_nx", "correlation")
metrics <- "auc_R_nx"
timeout <- 600

# start benchmark suite
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = scratch_file("suite/"),
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = timeout,
  memory = "16G",
  num_cores = 4,
  num_iterations = 5,
  num_repeats = 1,
  num_init_params = 16
)

outputs <- benchmark_suite_retrieve(scratch_file("suite/"))

outputs %>% rowwise() %>% mutate(memory = qacct$maxvmem) %>% ungroup()
outputs$qsub_error[[1]][[1]] %>% cat
