library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

best_parm <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets"))

methods <- get_descriptions(as_tibble = T)
metrics <- "auc_R_nx"
timeout <- 300

# start benchmark suite
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_file("suite/"),
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = timeout,
  memory = "11G",
  num_cores = 1,
  num_iterations = 100,
  num_repeats = 4,
  num_init_params = 200,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL
)


outputs <- benchmark_suite_retrieve(derived_file("suite/"))
