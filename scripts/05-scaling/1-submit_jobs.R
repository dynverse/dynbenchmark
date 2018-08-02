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
gen_gen_dataset <- function(lnrow, lncol) {
  f <- generate_dataset
  formals(f)$lnrow <- lnrow
  formals(f)$lncol <- lncol
  f
}

datasets <-
  seq(log10(100), log10(100000) - .4, by = log10(10) / 5) %>%
  crossing(lnrow = ., lncol = .) %>%
  as_tibble() %>%
  mutate(
    id = sprintf(paste0("scaling_%0", ceiling(log10(n())), "d"), seq_len(n())),
    type = "function",
    fun = pmap(lst(lnrow, lncol), gen_gen_dataset),
    nrow = ceiling(10 ^ lnrow),
    ncol = ceiling(10 ^ lncol),
    lsum = lnrow + lncol,
    memory = case_when(
      lsum >= 8 ~ "32G",
      lsum >= 6 ~ "10G",
      TRUE ~ "5G"
    )
  ) %>%
  select(id, type, fun, everything()) %>%
  filter(memory != "32G") # disable 32G for now

# define methods
method_ids <- dynmethods::methods$id
# method_ids <- c("scorpius", "identity")

# create design
design <- benchmark_generate_design(
  datasets = datasets,
  methods = method_ids
)

design$crossing <- design$crossing %>% left_join(datasets %>% select(dataset_id = id, memory), by = "dataset_id")

# save configuration
write_rds(design, derived_file("design.rds"))

benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}/{memory}",
  qsub_params = function(method_id, memory) list(timeout = 3600, memory = memory),
  metrics = list( dummy = function(dataset, model) { 1 } ),
  verbose = TRUE,
  output_models = FALSE
)
