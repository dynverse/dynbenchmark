library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/1-try_toys")

unlink(derived_file("", "5-optimise_parameters/1-try_toys"), recursive = TRUE, force = TRUE)

# collect 4 toy datasets
tasks <- read_rds(derived_file("tasks.rds", "5-optimise_parameters/0-process_tasks")) %>%
  filter(task_group == "toy") %>%
  sample_n(4)

# settings
methods <- get_descriptions()
metrics <- c("correlation", "rf_mse", "edge_flip")

# extract the default parameters
designs <- lapply(methods$par_set, function(par_set) {
  ParamHelpers::generateDesignOfDefaults(par_set)
}) %>% setNames(methods$short_name)

# save benchmark configuration and start it
bs_submit(
  tasks = tasks,
  task_group = tasks$task_group,
  task_fold = rep(1, nrow(tasks)),
  out_dir = derived_file("suite/"),
  remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
  methods = methods,
  designs = designs,
  metrics = metrics,
  memory = "10G",
  num_cores = 4,
  num_iterations = 1,
  num_repeats = 1,
  num_init_params = 1,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL,
  output_model = TRUE
)

outputs <- bs_retrieve(derived_file("suite/"))

outputs %>%
  select(method_name, error_message) %>%
  group_by(method_name, error_message) %>%
  summarise(n=n()) %>%
  filter(error_message != "")
