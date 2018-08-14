# define dataset function. two datasets with the same seed but different
# dimensionalities should have the same values in overlapping columns and rows.
generate_dataset <- function(orig_dataset_id, lnrow, lncol, seed = 1) {
  prev_seed <- .Random.seed[[1]]

  set.seed(seed)

  time0 <- Sys.time()

  nrow <- round(10 ^ lnrow)
  ncol <- round(10 ^ lncol)

  cat("SCALINGDATASET: Loading dataset\n")
  base_traj <- dynbenchmark::load_dataset(orig_dataset_id)
  base_traj <- base_traj %>% dynwrap::add_cell_waypoints(min(nrow, 400))
  base_geo <- dynwrap::compute_tented_geodesic_distances(base_traj)
  base_geo[!is.finite(base_geo)] <- 10 * max(base_geo[is.finite(base_geo)])
  base_knns <- FNN::get.knn(as.dist(base_geo), k = min(nrow - 1, 20))$nn.index


  counts <- dynwrap::get_expression(base_traj, "counts")

  if (ncol < nrow) {
    cat("SCALINGDATASET: Sampling ", ncol, " genes\n", sep = "")
    counts <- sapply(seq_len(ncol), function(i) {
      set.seed(i * seed)
      num_orig_genes <- pmax(rnorm(1, 5, 1), 3)
      ix <- sample.int(ncol(counts), num_orig_genes)
      new_c <- round(rowMeans(counts[,ix, drop = FALSE]))
      num_zero <- rowMeans(counts[,ix, drop = FALSE] == 0)
      new_c[num_zero > .2] <- 0
      new_c + rbinom(length(new_c), 100, .005)
      new_c
    })
  }

  cat("SCALINGDATASET: Sampling ", nrow, " cells\n", sep = "")
  set.seed(seed)
  cell_ixs <- sample.int(nrow(counts), nrow, replace = TRUE)

  counts <- t(sapply(seq_len(nrow), function(i) {
    set.seed(i * seed)
    j <- cell_ixs[[i]]
    num_orig_cells <- round(runif(1, 3, ncol(base_knns)))
    ix <- base_knns[j, sample.int(ncol(base_knns), num_orig_cells)]
    new_c <- round(colMeans(counts[ix, , drop = FALSE]))
    num_zero <- colMeans(counts[ix, , drop = FALSE] == 0)
    new_c[num_zero > .2] <- 0
    new_c + rbinom(length(new_c), 100, .005)
    new_c
  }))


  if (ncol >= nrow) {
    cat("SCALINGDATASET: Sampling ", ncol, " genes\n", sep = "")
    counts <- sapply(seq_len(ncol), function(i) {
      set.seed(i * seed)
      num_orig_genes <- pmax(rnorm(1, 5, 1), 3)
      ix <- sample.int(ncol(counts), num_orig_genes)
      new_c <- round(rowMeans(counts[,ix, drop = FALSE]))
      num_zero <- rowMeans(counts[,ix, drop = FALSE] == 0)
      new_c[num_zero > .2] <- 0
      new_c + rbinom(length(new_c), 100, .005)
      new_c
    })
  }

  cat("SCALINGDATASET: format counts and expression\n")
  expression <- log2(counts + 1)

  cell_ids <- paste0("Cell", seq_len(nrow))
  gene_ids <- paste0("Gene", seq_len(ncol))
  dimnames(counts) <- dimnames(expression) <- list(cell_ids, gene_ids)

  cat("SCALINGDATASET: create progressions\n")
  progressions <- base_traj$progressions %>%
    slice(match(cell_id, base_traj$cell_ids)) %>% # there is no tenting so this should not be an issue
    slice(cell_ixs) %>%
    mutate(cell_id = cell_ids)

  cat("SCALINGDATASET: wrapping trajectory and adding prior information\n")
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

  cat("SCALINGDATASET: returning trajectory\n")
  set.seed(prev_seed)

  time1 <- Sys.time()

  cat("SCALINGDATASET: time elapsed ", round(as.numeric(difftime(time1, time0, units = "secs")), 2), " s\n", sep = "")

  dataset
}
