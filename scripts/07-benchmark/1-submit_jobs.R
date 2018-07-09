library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

# settings
methods <- get_ti_methods(packages = c("dynwrap", "dynmethods")) %>% filter(short_name %in% c("scorpius", "embeddr"))
metrics <- c("correlation", "rf_mse", "edge_flip", "featureimp_cor")
timeout_per_execution <- 60 * 60 * 6
num_repeats <- 4
execute_before <- "export DYNBENCHMARK_PATH=/group/irc/shared/dynbenchmark/; singularity exec -B /scratch:/scratch -B /group:/group /scratch/irc/shared/dynbenchmark.simg \\"
verbose <- TRUE

# run most methods
max_memory_per_execution <- "8G"
method_filter <- c("mnclica", "recat", "ctgibbs", "scimitar", "ouijaflw", "ouija", "pseudogp")

# run methods that require more memory
# max_memory_per_execution <- "32G"
# method_filter <- c("ouija", "pseudogp")

# # execute ouija and pseudogp last because they will jam up other methods
# max_memory_per_execution <- "8G"
# method_filter <- c()


# define important folders
local_output_folder <- derived_file("suite/")
remote_output_folder <- paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynbenchmark_experiment_id"), "/")
dataset_ids <- list_datasets() %>% head(10)

# use previous output to determine method ordering based on its running time
# read_rds("analysis/data/derived_data/06-optimise_parameters-previousresults/180226-derived_data/3-evaluate_parameters/outputs_postprocessed.rds") %>%
# # read_rds("analysis/data/derived_data/06-optimise_parameters/3-evaluate_parameters/outputs_postprocessed.rds") %>%
#   .$outputs_summmethod %>%
#   group_by(method_short_name) %>%
#   summarise_if(is.numeric, sum) %>%
#   arrange(time_method) %>%
#   .$method_short_name %>%
#   paste("\"", ., "\"", collapse = ", ", sep = "") %>%
#   cat
methods_order <- c(
  "identity", "shuffle", "random", "manual_wouters", "manual_robrechtc", "slngsht", "mpath",  "comp1", "angle", "periodpc", "gng",
  "waterfll", "tscan", "sincell", "scorpius", "scorspar", "embeddr", "wndrlst", "wishbone", "mnclddr", "dpt", "mnclica", "slice",
  "ctvem", "ouijaflw", "slicer", "scuba", "topslam", "gpfates", "aga", "agapt", "phenopth", "ctmaptpx", "mfa", "stemid2", "recat",
  "stemid", "scoup", "ctgibbs", "scimitar", "ouija", "pseudogp"
)
methods <- methods %>% slice(c(match(methods_order, methods$short_name), which(!methods$short_name %in% methods_order)))
methods$short_name

methods <- methods %>% filter(!short_name %in% method_filter)

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
