library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

# settings
methods <- get_ti_methods(ti_packages = c("dynwrap", "dynmethods"))
metrics <- list( dummy = function(dataset, model) { 1 } )
timeout_per_execution <- 60 * 60
num_repeats <- 1
execute_before <- ""
# execute_before <- "export DYNBENCHMARK_PATH=/group/irc/shared/dynbenchmark/; singularity exec -B /scratch:/scratch -B /group:/group /scratch/irc/shared/dynbenchmark.simg \\"
verbose <- TRUE

# run most methods
# max_memory_per_execution <- "8G"
# method_filter <- c("mnclica", "recat", "ctgibbs", "scimitar", "ouijaflw", "ouija", "pseudogp")

# run methods that require more memory
# max_memory_per_execution <- "32G"
# method_filter <- c("ouija", "pseudogp")

# # execute ouija and pseudogp last because they will jam up other methods
max_memory_per_execution <- "8G"
method_filter <- c()


# define important folders
local_output_folder <- derived_file("suite/")
remote_output_folder <- derived_file("suite/", remote = TRUE)
dataset_ids <- "synthetic/linear_5"

methods <- methods %>% filter(!method_id %in% method_filter)

# extract the default parameters
parameters <- lapply(methods$method_func, function(fun) {
  par_set <- fun()$par_set
  defaults <- ParamHelpers::generateDesignOfDefaults(par_set) %>% mutate(paramset_id = "default")
  defaults
}) %>% setNames(methods$method_id)

# save benchmark configuration and start it
write_rds(lst(
  methods, metrics, timeout_per_execution, num_repeats, max_memory_per_execution, execute_before, verbose,
  local_output_folder, remote_output_folder, dataset_ids, parameters
), derived_file("config.rds"))

benchmark_submit(
  dataset_ids = dataset_ids,
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
