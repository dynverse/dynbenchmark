library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/2-parameter_optimisation")

# settings
methods <- get_descriptions() %>% filter(short_name != "manual")
metrics <- c("correlation", "rf_mse", "edge_flip")
timeout_paramoptim <- 1 * 24 * 60 * 60
num_repeats <- 4
num_init_params <- 16
num_iterations <- 1000
num_cores <- 8
max_memory_per_core <- "10G"
execute_before <- "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500; export DYNALYSIS_PATH=/group/irc/shared/dynalysis/"
verbose <- TRUE

# define important folders
local_tasks_folder <- derived_file("tasks", "5-optimise_parameters/0-process_tasks")
remote_tasks_folder <- "/scratch/irc/shared/dynverse_derived/5-optimise_parameters/0-process_tasks/tasks"
local_output_folder <- derived_file("suite/")
remote_output_folder <- paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/")
task_ids <- read_rds(paste0(local_tasks_folder, "/task_ids.rds")) %>% keep(~str_detect(., "synthetic/"))

methods_order <- c(
  "identity", "shuffle", "random", "slngsht", "mpath", "waterfll", "sincell", "tscan", "scorpius",
  "embeddr", "dpt", "wndrlst", "wishbone", "mnclddr", "mnclica", "slice", "ouijaflw", "ctvem",
  "scimitar", "slicer", "scuba", "topslam", "gpfates", "phenopth", "pseudogp", "recat", "stemid",
  "ctmaptpx", "ouija", "mfa", "scoup", "ctgibbs"
)
# methods <- methods %>% slice(c(match(methods_order, methods$short_name)
, which(!methods$short_name %in% methods_order)))
methods <- methods %>% slice(match(methods_order, methods$short_name))
methods$short_name

# save benchmark configuration and start it
write_rds(lst(
  methods, metrics, timeout_paramoptim, num_repeats, num_init_params, num_iterations, max_memory_per_core, num_cores,
  execute_before, verbose, local_tasks_folder, remote_tasks_folder, local_output_folder, remote_output_folder, task_ids
), derived_file("config.rds"))

paramoptim_submit(
  task_ids = task_ids,
  local_tasks_folder = local_tasks_folder,
  remote_tasks_folder = remote_tasks_folder,
  methods = methods,
  timeout_paramoptim = timeout_paramoptim,
  max_memory_per_core = max_memory_per_core,
  num_cores = num_cores,
  metrics = metrics,
  num_repeats = num_repeats,
  num_iterations = num_iterations,
  num_init_params = num_init_params,
  local_output_folder = local_output_folder,
  remote_output_folder = remote_output_folder,
  execute_before = execute_before,
  verbose = verbose
)
