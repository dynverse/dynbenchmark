library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

# define dataset function. two datasets with the same seed but different
# dimensionalities should have the same values in overlapping columns and rows.
generate_dataset <- function(lnrow, lncol, seed = 1) {
  prev_seed <- .Random.seed[[1]]

  set.seed(seed)

  cat("TOY: Generating dyntoy/tree model\n")
  base_traj <- dyntoy::generate_dataset(
    id = "dyntoy",
    model = "tree",
    num_cells = 400,
    num_features = 400,
    allow_tented_progressions = FALSE,
    normalise = FALSE,
    add_prior_information = FALSE
  ) %>% add_cell_waypoints(400)
  base_geo <- dynwrap::compute_tented_geodesic_distances(base_traj)
  base_knns <- FNN::get.knn(as.dist(base_geo), k = 10)$nn.index

  nrow <- round(10 ^ lnrow)
  ncol <- round(10 ^ lncol)

  orig_counts <- base_traj$counts

  cat("TOY: Sampling ", ncol, " genes\n", sep = "")
  counts_more_genes <- sapply(seq_len(ncol), function(i) {
    set.seed(i * seed)
    num_orig_genes <- pmax(rnorm(1, 5, 1), 3)
    ix <- sample.int(ncol(orig_counts), num_orig_genes)
    new_c <- round(rowMeans(orig_counts[,ix, drop = FALSE]))
    num_zero <- rowMeans(orig_counts[,ix, drop = FALSE] == 0)
    new_c[num_zero > .2] <- 0
    new_c
  })

  cat("TOY: Sampling ", nrow, " cells\n", sep = "")
  set.seed(seed)
  cell_ixs <- sample.int(nrow(orig_counts), nrow, replace = TRUE)

  counts <- t(sapply(seq_len(nrow), function(i) {
    set.seed(i * seed)
    j <- cell_ixs[[i]]
    num_orig_cells <- round(runif(1, 3, ncol(base_knns)))
    ix <- base_knns[j, sample.int(ncol(base_knns), num_orig_cells)]
    new_c <- round(colMeans(counts_more_genes[ix, , drop = FALSE]))
    num_zero <- colMeans(counts_more_genes[ix, , drop = FALSE] == 0)
    new_c[num_zero > .2] <- 0
    new_c
  }))

  cat("TOY: format counts and expression\n")
  expression <- log2(counts + 1)

  cell_ids <- paste0("Cell", seq_len(nrow))
  gene_ids <- paste0("Gene", seq_len(ncol))
  dimnames(counts) <- dimnames(expression) <- list(cell_ids, gene_ids)

  cat("TOY: create progressions\n")
  progressions <- base_traj$progressions %>%
    slice(match(cell_id, base_traj$cell_ids)) %>% # there is no tenting so this should not be an issue
    slice(cell_ixs) %>%
    mutate(cell_id = cell_ids)

  cat("TOY: wrapping trajectory and adding prior information\n")
  set.seed(seed)
  dataset <-
    dynwrap::wrap_data(
      id = paste0("scaling_", lnrow, "_", lncol),
      cell_ids = cell_ids
    ) %>%
    dynwrap::add_trajectory(
      milestone_ids = base_traj$milestone_ids,
      milestone_network = base_traj$milestone_network,
      progressions = progressions
    ) %>%
    dynwrap::add_expression(
      counts = counts,
      expression = expression
    ) %>%
    dynwrap::add_prior_information() %>%
    dynwrap::add_cell_waypoints(100) %>%
    dynwrap::add_root("Cell1")

  cat("TOY: returning toy trajectory")
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
  select(id, type, fun, everything())

# define methods
checks <- read_rds(derived_file("checks.rds", experiment_id = "05-method_testing"))


cat("NOT RUNNING: ", checks %>% filter(ran == 0) %>% pull(method_id) %>% paste(collapse = ", "), "\n", sep = "")
method_ids <- checks %>% filter(ran > 0) %>% pull(method_id)
# method_ids <- dynmethods::methods$id
# method_ids <- c("scorpius", "identity", "error", "embeddr") # test run
methods <-
  dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
  mapdf(function(m) {
    l <- m$method_func()
    l$fun <- m$method_func
    l$type <- "function"
    l
  }) %>%
  list_as_tibble() %>%
  select(id, type, fun, everything())

# create design
design <- benchmark_generate_design(
  datasets = datasets,
  methods = methods
)

more_mem <- c("calista", "cellrouter", "grandprix", "ouijaflow")

design$crossing <- design$crossing %>%
  left_join(datasets %>% select(dataset_id = id, memory), by = "dataset_id") %>%
  mutate(
    orig_memory = memory,
    memory = case_when(
      method_id %in% more_mem & memory == "5G" ~ "10G",
      method_id %in% more_mem & memory == "10G" ~ "20G",
      TRUE ~ memory
    )
  )

# save configuration
write_rds(design, derived_file("design.rds"))

design_filt <- read_rds(derived_file("design.rds"))

design_filt$crossing <- design_filt$crossing %>% filter(orig_memory %in% c("5G", "10G")) # disable larger networks for now

benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{memory}",
  qsub_params = function(method_id, memory) list(timeout = 3600, memory = memory),
  metrics = list(dummy = function(dataset, model) 1),
  verbose = TRUE,
  output_models = FALSE
)
