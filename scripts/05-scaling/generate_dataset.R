# define dataset function. two datasets with the same seed but different
# dimensionalities should have the same values in overlapping columns and rows.
generate_dataset <- function(lnrow, lncol, seed = 1) {
  prev_seed <- .Random.seed[[1]]

  set.seed(seed)

  time0 <- Sys.time()

  cat("TOY: Generating dyntoy/tree model\n")
  base_traj <- dyntoy::generate_dataset(
    id = "dyntoy",
    model = "tree",
    num_cells = 400,
    num_features = 400,
    allow_tented_progressions = FALSE,
    normalise = FALSE,
    add_prior_information = FALSE
  ) %>% dynwrap::add_cell_waypoints(400)
  base_geo <- dynwrap::compute_tented_geodesic_distances(base_traj)
  base_knns <- FNN::get.knn(as.dist(base_geo), k = 20)$nn.index

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
    new_c + rbinom(length(new_c), 100, .005)
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
    new_c + rbinom(length(new_c), 100, .005)
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

  time1 <- Sys.time()

  cat("TOY: time elapsed ", round(as.numeric(difftime(time1, time0, units = "secs")), 2), " s\n", sep = "")

  dataset
}
