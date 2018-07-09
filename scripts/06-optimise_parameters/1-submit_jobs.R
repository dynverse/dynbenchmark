library(dynbenchmark)
library(tidyverse)

experiment("06-optimise_parameters")

# settings
methods <- get_ti_methods(packages = c("dynwrap", "dynmethods")) %>% filter(!short_name %in% c("manual", "pseudogp", "slicer", "stemid", "ouija", "stemid2"))
metrics <- c("correlation", "rf_mse", "edge_flip")
timeout_paramoptim <- 1 * 24 * 60 * 60
num_repeats <- 4
num_init_params <- 16
num_iterations <- 1000
num_cores <- 8
max_memory_per_core <- "10G"
execute_before <- "export DYNBENCHMARK_PATH=/group/irc/shared/dynbenchmark/; singularity exec -B /scratch:/scratch -B /group:/group /scratch/irc/shared/dynmethods.simg \\"
verbose <- TRUE

# define important folders
dataset_ids <- load_datasets() %>% filter(dataset_source == "synthetic") %>% pull(id)
local_output_folder <- derived_file("suite/")
remote_output_folder <- paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynbenchmark_experiment_id"), "/")

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
  execute_before, verbose, local_output_folder, remote_output_folder, dataset_ids
), derived_file("config.rds"))

paramoptim_submit(
  dataset_ids = dataset_ids,
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
