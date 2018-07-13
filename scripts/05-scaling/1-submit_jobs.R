library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

# define datasets
dataset_ids <- c("synthetic/linear_5", "synthetic/linear_6")

# define methods
method_ids <- get_ti_methods(ti_packages = c("dynmethods"))$method_id # removed dynwrap for now

# create design
design <- generate_benchmark_design(
  dataset_ids = dataset_ids,
  method_ids = method_ids
)

# define other parameters
metrics <- list( dummy = function(dataset, model) { 1 } )
timeout_per_execution <- 60 * 60
execute_before <- ""
# execute_before <- "export DYNBENCHMARK_PATH=/group/irc/shared/dynbenchmark/; singularity exec -B /scratch:/scratch -B /group:/group /scratch/irc/shared/dynbenchmark.simg \\"
verbose <- TRUE
max_memory_per_execution <- "8G"
local_output_folder <- derived_file("suite/")
remote_output_folder <- derived_file("suite/", remote = TRUE)

# save configuration
write_rds(lst(
  design, metrics, timeout_per_execution,
  max_memory_per_execution, execute_before, verbose,
  local_output_folder, remote_output_folder
), derived_file("config.rds"))

benchmark_submit(
  design = design,
  timeout_per_execution = timeout_per_execution,
  max_memory_per_execution = max_memory_per_execution,
  metrics = metrics,
  local_output_folder = local_output_folder,
  remote_output_folder = remote_output_folder,
  execute_before = execute_before,
  verbose = verbose
)
