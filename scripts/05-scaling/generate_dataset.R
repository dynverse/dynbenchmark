#' Helper to generate an up- and downscaled dataset which looks similar to the original datasets

# define dataset function. two datasets with the same seed but different
# dimensionalities should have the same values in overlapping columns and rows.
generate_dataset <- function(
  orig_dataset_id,
  lnrow,
  lncol,
  seed = 1,
  cores = 1,
  verbose = TRUE
) {
  prev_seed <- .Random.seed[[1]]

  set.seed(seed)

  time0 <- Sys.time()

  nrow <- round(10 ^ lnrow)
  ncol <- round(10 ^ lncol)

  expand_fun <- function(counts, margin, new_margin_size, knns, seed, must_have = c()) {
    dim_margin <- if (margin == 1) nrow(counts) else ncol(counts)

    set.seed(seed)
    ref_ixs <- c(must_have, sample.int(dim_margin, new_margin_size - length(must_have), replace = TRUE))

    vecs <- pbapply::pblapply(seq_len(new_margin_size), cl = cores, function(i) {
      set.seed(i * seed)
      ref_ix <- ref_ixs[[i]]
      num_secs <- rnorm(1, 5, 1) %>% pmax(3) %>% pmin(ncol(knns)) %>% round()
      sec_ixs <- sample(knns[ref_ix, ], num_secs)
      sec_weights <- runif(num_secs) %>% {. / sum(.)}

      ref_vals <- if (margin == 1) counts[ref_ix, , drop = TRUE] else counts[, ref_ix, drop = TRUE]
      vals <- if (margin == 1) counts[sec_ixs, ] else counts[, sec_ixs]
      sec_vals <- vals %>%
        sweep(MARGIN = margin, STATS = sec_weights, FUN = "*") %>%
        apply(MARGIN = 3 - margin, FUN = sum)

      sort(ref_vals)[order(order(sec_vals))] %>% set_names(NULL)
    })
    bind_fun <- if (margin == 1) rbind else cbind
    out <- do.call(bind_fun, vecs)
    attr(out, "ref_ixs") <- ref_ixs
    out
  }

  if (verbose) cat("SCALINGDATASET: Loading dataset\n")
  base_traj <- dynbenchmark::load_dataset(orig_dataset_id)
  base_traj <- base_traj %>% dynwrap::add_cell_waypoints(min(nrow, 100))

  counts <- dynwrap::get_expression(base_traj, "counts")
  expression <- dynwrap::get_expression(base_traj, "expression")

  must_have_cells <- match(base_traj$prior_information %>% {c(.$start_id, .$end_id)} %>% unique(), base_traj$cell_ids)

  space_cells <- dyndimred::dimred_landmark_mds(expression, ndim = 10)
  cell_knns <- FNN::get.knn(space_cells, k = 10)$nn.index
  space_genes <- dyndimred::dimred_landmark_mds(t(expression), ndim = 10)
  gene_knns <- FNN::get.knn(space_genes, k = 10)$nn.index

  if (lncol < lnrow) {
    if (verbose) cat("SCALINGDATASET: Sampling ", ncol, " genes\n", sep = "")
    counts <- expand_fun(counts = counts, new_margin_size = ncol, margin = 2, knns = gene_knns, seed = seed)
  }

  if (verbose) cat("SCALINGDATASET: Sampling ", nrow, " cells\n", sep = "")
  counts <- expand_fun(counts = counts, new_margin_size = nrow, margin = 1, knns = cell_knns, seed = seed, must_have = must_have_cells)
  cell_ixs <- attr(counts, "ref_ixs")

  if (lncol >= lnrow) {
    cat("SCALINGDATASET: Sampling ", ncol, " genes\n", sep = "")
    counts <- expand_fun(counts = counts, new_margin_size = ncol, margin = 2, knns = gene_knns, seed = seed)
  }

  set.seed(1)
  counts[seq_len(nrow), 1] <- sample.int(nrow)
  counts[1, seq_len(ncol)] <- sample.int(ncol)

  if (verbose) cat("SCALINGDATASET: Format counts and expression\n")
  expression <- log2(counts + 1)

  cell_ids <- paste0("Cell", seq_len(nrow))
  gene_ids <- paste0("Gene", seq_len(ncol))
  dimnames(counts) <- dimnames(expression) <- list(cell_ids, gene_ids)
  cell_id_map <- data_frame(old_id = base_traj$cell_ids[cell_ixs], cell_id = cell_ids)

  if (verbose) cat("SCALINGDATASET: create progressions\n")
  progressions <-
    base_traj$progressions %>%
    rename(old_id = cell_id) %>%
    right_join(cell_id_map, by = "old_id") %>%
    select(-old_id)

  if (verbose) cat("SCALINGDATASET: determine prior information\n")
  prio <- base_traj$prior_information
  matching_cell_ids <- base_traj$cell_ids[cell_ixs]
  features_id <- gene_ids
  start_id <- cell_ids[match(prio$start_id, matching_cell_ids)]
  end_id <- cell_ids[match(prio$end_id, matching_cell_ids)]
  groups_id <- prio$groups_id %>%
    rename(old_id = cell_id) %>%
    right_join(cell_id_map, by = "old_id") %>%
    select(-old_id)
  groups_network <- prio$groups_network
  groups_n <- prio$groups_n
  timecourse_continuous <- prio$timecourse_continuous[matching_cell_ids] %>% set_names(cell_ids)
  timecourse_discrete <- prio$timecourse_discrete[matching_cell_ids] %>% set_names(cell_ids)

  if (verbose) cat("SCALINGDATASET: wrapping trajectory and adding prior information\n")
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
    dynwrap::add_prior_information(
      start_id = start_id,
      end_id = end_id,
      groups_id = groups_id,
      groups_network = groups_network,
      groups_n = groups_n,
      features_id = features_id,
      timecourse_continuous = timecourse_continuous,
      timecourse_discrete = timecourse_discrete,
      verbose = TRUE
    ) %>%
    dynwrap::add_cell_waypoints(100) %>%
    dynwrap::add_root("Cell1")

  if (verbose) cat("SCALINGDATASET: returning trajectory\n")
  set.seed(prev_seed)

  time1 <- Sys.time()

  if (verbose) cat("SCALINGDATASET: time elapsed ", round(as.numeric(difftime(time1, time0, units = "secs")), 2), " s\n", sep = "")

  dataset
}
