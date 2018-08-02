library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

# settings
method_ids <- dynmethods::methods$id %>% keep(~ . != c("calista", "ouija", "pseudogp"))
metrics <- c("correlation", "rf_mse", "edge_flip", "featureimp_cor")
qsub_params <- function(method_id, param_id) {
  prm <- lst(timeout = 6 * 60 * 60, memory = "8G")
  if (method_id %in% c("ctgibbs", "scimitar", "ouijaflow", "ouija", "pseudogp")) {
    prm$memory <- "32G"
  }
  prm
}

num_repeats <- 4
verbose <- TRUE

dataset_ids <- list_datasets()$dataset_id

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
