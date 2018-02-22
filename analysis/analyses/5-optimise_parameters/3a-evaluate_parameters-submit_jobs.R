library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/3-evaluate_parameters")

# settings
methods <- get_descriptions() %>% filter(short_name != "manual")
metrics <- c("correlation", "rf_mse", "edge_flip")
timeout_per_execution <- 60 * 60 * 6
num_repeats <- 4
max_memory_per_execution <- "32G"
execute_before <- "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500; export DYNALYSIS_PATH=/group/irc/shared/dynalysis/"
verbose <- TRUE

# define important folders
local_tasks_folder <- derived_file("tasks", "5-optimise_parameters/0-process_tasks")
remote_tasks_folder <- "/scratch/irc/shared/dynverse_derived/5-optimise_parameters/0-process_tasks/tasks"
local_output_folder <- derived_file("suite/")
remote_output_folder <- paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/")
task_ids <- read_rds(paste0(local_tasks_folder, "/task_ids.rds"))

# use previous output to determine method ordering based on its running time
# outs <- read_rds("analysis/data/derived_data/5-optimise_parameters-previousresults/180220-derived_data/3-evaluate_parameters/outputs_postprocessed.rds")
# outs$outputs_summmethod %>%
#   group_by(method_short_name) %>%
#   summarise_if(is.numeric, sum) %>%
#   arrange(time_method_mean) %>%
#   .$method_short_name %>%
#   paste("\"", ., "\"", collapse = ", ", sep = "") %>%
#   cat
methods_order <- c(
  "identity", "scimitar", "shuffle", "random", "manual", "slngsht", "mpath", "waterfll", "sincell", "tscan", "scorpius",
  "embeddr", "dpt", "wndrlst", "mnclddr", "wishbone", "slice", "mnclica", "ctvem", "ouijaflw", "slicer", "recat", "gpfates",
  "topslam", "scuba", "phenopth", "ctmaptpx", "mfa", "stemid", "ctgibbs", "scoup", "pseudogp", "ouija"
)
methods <- methods %>% slice(c(match(methods_order, methods$short_name), which(!methods$short_name %in% methods_order)))
methods$short_name

# extract the default parameters
parameters <- lapply(methods$short_name, function(mn) {
  par_set <- methods %>% filter(short_name == mn) %>% .$par_set %>% .[[1]]
  defaults <- ParamHelpers::generateDesignOfDefaults(par_set) %>% mutate(paramset_id = "default")
  # best <- ... %>% mutate(paramset_id = "optimised)
  # bind_rows(defaults, best)
  defaults
}) %>% setNames(methods$short_name)

# parameters$manual <-
#   tribble(
#     ~person_id, ~dimred_id, ~run_i, ~paramset_id,
#     "wouters", "pca", "1", "wouters",
#     "robrechtc", "mds", "1", "robrechtc"
#   )

# save benchmark configuration and start it
write_rds(lst(
  methods, metrics, timeout_per_execution, num_repeats, max_memory_per_execution, execute_before, verbose, local_tasks_folder, remote_tasks_folder,
  local_output_folder, remote_output_folder, task_ids, parameters
), derived_file("config.rds"))

benchmark_submit(
  task_ids = task_ids,
  local_tasks_folder = local_tasks_folder,
  remote_tasks_folder = remote_tasks_folder,
  methods = methods,
  parameters = parameters,
  timeout_per_execution = timeout_per_execution,
  max_memory_per_execution = max_memory_per_execution,
  metrics = metrics,
  num_repeats = num_repeats,
  local_output_folder = local_output_folder,
  remote_output_folder = remote_output_folder,
  execute_before = execute_before,
  verbose = verbose
)
