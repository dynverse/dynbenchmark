library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters")

# settings
methods <- get_descriptions() %>% filter(!short_name %in% c("manual", "pseudogp", "slicer", "stemid", "ouija", "stemid2"))
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
task_ids <- load_datasets_tibble() %>% filter(task_source == "synthetic") %>% pull(id)
local_output_folder <- derived_file("suite/")
remote_output_folder <- paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/")

methods_order <- c(
  "identity", "shuffle", "random", "slngsht", "mpath",  "comp1", "angle", "periodpc", "gng",
  "waterfll", "tscan", "sincell", "scorpius", "scorspar", "embeddr"
  # , "wndrlst", "wishbone", "mnclddr", "dpt", "mnclica", "slice",
  # "ctvem", "ouijaflw", "slicer", "scuba", "topslam", "gpfates", "paga", "agapt", "phenopth", "ctmaptpx", "mfa", "stemid2", "recat",
  # "stemid", "scoup", "ctgibbs", "scimitar", "ouija", "pseudogp"
)
# methods <- methods %>% slice(c(match(methods_order, methods$short_name), which(!methods$short_name %in% methods_order)))
methods <- methods %>% slice(match(methods_order, methods$short_name))
methods$short_name

# save benchmark configuration and start it
write_rds(lst(
  methods, metrics, timeout_paramoptim, num_repeats, num_init_params, num_iterations, max_memory_per_core, num_cores,
  execute_before, verbose, local_output_folder, remote_output_folder, task_ids
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
