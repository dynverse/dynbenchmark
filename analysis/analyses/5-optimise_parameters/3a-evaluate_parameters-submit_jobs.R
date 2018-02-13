library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/3-evaluate_parameters")

# settings
methods <- get_descriptions()
metrics <- c("correlation", "rf_mse", "edge_flip")
extra_metrics <- c()
eval_timeout <- 60 * 60
optim_timeout <- 7 * 24 * 60 * 60
num_repeats <- 4

# read tasks
tasks <- read_rds(derived_file("tasks.rds", "5-optimise_parameters/0-process_tasks")) %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol)) %>%
  filter(nrow < 2000)

# extract the default parameters
designs <- lapply(methods$short_name, function(mn) {
  par_set <- methods %>% filter(short_name == mn) %>% .$par_set %>% .[[1]]
  defaults <- ParamHelpers::generateDesignOfDefaults(par_set)
  # best <- ...
  # bind_rows(defaults, best)
  defaults
}) %>% setNames(methods$short_name)

designs$manual <-
  tribble(
    ~person_id, ~dimred_id, ~run_i,
    "wouters", "pca", 1,
    "robrechtc", "mds", 1
  )

# save benchmark configuration and start it
write_rds(lst(methods, designs, metrics, extra_metrics, num_repeats, tasks), derived_file("config.rds"))
bs_submit(
  tasks = tasks,
  task_group = rep("task_group", nrow(tasks)),
  task_fold = rep(1, nrow(tasks)),
  out_dir = derived_file("suite/"),
  remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
  methods = methods,
  designs = designs,
  metrics = metrics,
  extra_metrics = extra_metrics,
  memory = "10G",
  num_cores = 4,
  num_iterations = 1,
  num_repeats = num_repeats,
  num_init_params = num_init_params,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500; export DYNALYSIS_PATH=/group/irc/shared/dynalysis/",
  r_module = NULL,
  output_model = TRUE
)
