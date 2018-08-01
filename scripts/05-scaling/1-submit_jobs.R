library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

# define dataset function. two datasets with the same seed but different
# dimensionalities should have the same values in overlapping columns and rows.
generate_dataset <- function(lnrow, lncol, seed = 1) {
  prev_seed <- .Random.seed[[1]]
  nrow <- ceiling(10 ^ lnrow)
  ncol <- ceiling(10 ^ lncol)
  expression <- sapply(seq_len(ncol), function(i) {
    set.seed(i * seed)
    runif(nrow)
  })
  expression[expression <= .9] <- 0
  counts <- round(2^expression) + 1

  cell_ids <- paste0("Cell", seq_len(nrow))
  gene_ids <- paste0("Gene", seq_len(nrow))
  dimnames(counts) <- dimnames(expression) <- list(cell_ids, gene_ids)

  set.seed(seed)
  dataset <-
    dynwrap::wrap_data(
      id = paste0("scaling_", lnrow, "_", lncol),
      cell_ids = cell_ids
    ) %>%
    dynwrap::add_trajectory(
      milestone_network = data_frame(from = "A", to = "B", length = 1, directed = FALSE),
      progressions = data_frame(cell_id = cell_ids, from = "A", to = "B", percentage = runif(length(cell_ids)))
    ) %>%
    dynwrap::add_expression(
      counts = counts,
      expression = expression
    ) %>%
    dynwrap::add_prior_information() %>%
    dynwrap::add_cell_waypoints() %>%
    dynwrap::add_root("Cell1")

  set.seed(prev_seed)

  dataset
}

dim_test <- seq(log10(100), log10(100000), by = log10(10) / 4)
dim_df <- crossing(
  nrow = dim_test,
  ncol = dim_test
)
dataset_funs <- lapply(seq_len(nrow(dim_df)), function(i) {
  fun <- generate_dataset
  formals(fun)$lnrow <- dim_df$nrow[[i]]
  formals(fun)$lncol <- dim_df$ncol[[i]]
  fun
})


# define methods
method_ids <- dynmethods::methods$id

# create design
design <- benchmark_generate_design(
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
