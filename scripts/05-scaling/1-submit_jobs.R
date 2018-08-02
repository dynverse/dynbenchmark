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
  gene_ids <- paste0("Gene", seq_len(ncol))
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
gen_gen_dataset <- function(nrow, ncol) {
  f <- generate_dataset
  formals(f)$lnrow <- nrow
  formals(f)$lncol <- ncol
  f
}

datasets <-
  seq(log10(100), log10(100000), by = log10(10) / 4) %>%
  crossing(nrow = ., ncol = .) %>%
  as_tibble() %>%
  mutate(
    id = sprintf(paste0("scaling_%0", ceiling(log10(n())), "d"), seq_len(n())),
    type = "function",
    fun = pmap(lst(nrow, ncol), gen_gen_dataset)
  ) %>%
  select(id, type, fun, everything())

# define methods
# method_ids <- dynmethods::methods$id
method_ids <- "scorpius"

# create design
design <- benchmark_generate_design(
  datasets = datasets,
  methods = method_ids
)

# save configuration
write_rds(design, derived_file("design.rds"))

benchmark_submit(
  design = design,
  qsub_params = list(timeout = 3600, memory = "8G"),
  metrics = list( dummy = function(dataset, model) { 1 } ),
  verbose = TRUE,
  output_models = FALSE
)
