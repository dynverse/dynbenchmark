#' Process a raw real or synthetic dataset
#'
#' @inheritParams dynwrap::wrap_data
#' @inheritParams dynwrap::add_expression
#' @inheritParams dynwrap::add_cluster_graph
#' @param root_milestone_id The root milestone, optional
#'
#' @importFrom dynnormaliser normalise_filter_counts
#' @importFrom dynwrap generate_prior_information
#' @importFrom testthat expect_match
#'
#' @export
process_raw_dataset <- function(
  id,
  counts,
  milestone_network,
  grouping,
  root_milestone_id = NULL,
  cell_info = tibble(cell_id = rownames(counts)),
  feature_info = tibble(feature_id = colnames(counts))
) {
  testthat::expect_match(id, ".+/.+")

  # convert symbols
  conversion_out <- convert_to_symbol(counts)
  counts_prefilter <- conversion_out$counts
  feature_info <- feature_info %>% filter(conversion_out$filtered) %>% mutate(feature_id = colnames(counts_prefilter))

  # normalise and filter expression
  norm_out <- dynnormaliser::normalise_filter_counts(
    counts_prefilter,
    verbose = TRUE
  )

  normalisation_info <- norm_out$info
  expression <- norm_out$expression
  counts <- norm_out$counts

  # filter cells based on normalisation
  cell_ids <- rownames(counts)
  cell_info <- cell_info %>% slice(match(cell_ids, cell_id))
  grouping <- grouping[cell_ids]
  feature_info <- feature_info %>% slice(match(colnames(counts), feature_id))

  # wrap dataset
  dataset <- dynwrap::wrap_data(
    dataset_source = str_replace(id, "/.*", ""),
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    normalisation_info = normalisation_info,
    creation_date = Sys.time()
  ) %>%
    dynwrap::add_cluster_graph(
    milestone_network = milestone_network,
    grouping = grouping
  ) %>%
    dynwrap::add_expression(
    counts = counts,
    expression = expression,
    feature_info = feature_info
  ) %>%
    dynwrap::add_prior_information() %>%
    dynwrap::add_cell_waypoints()

  # add root if given, or choose default root if not given
  if (is.null(root_milestone_id)) {
    root_milestone_id <- milestone_network$from[[1]]
  }
  dataset <- dynwrap::add_root(dataset, root_milestone_id = root_milestone_id)

  # save the dataset
  save_dataset(dataset, dataset_id = id)

  # save the counts before filtering/normalisation
  write_rds(counts_prefilter, dataset_file(dataset_id = id, filename = "counts_prefilter.rds"))

  # save the normalisation plots
  pdf(dataset_file(dataset_id = id, "normalisation.pdf"))
  walk(norm_out$normalisation_plots, print)
  graphics.off()

  return(invisible())
}

convert_to_symbol <- function(counts) {
  colnames(counts) <- tibble(gene_id = colnames(counts)) %>%
    left_join(dynbenchmark::id_mapper, by = c("gene_id" = "ensembl")) %>%
    mutate(gene_id = ifelse(is.na(symbol), gene_id, symbol)) %>%
    pull(gene_id)
  filtered <- colnames(counts) %in% names(which(table(colnames(counts)) == 1))
  counts <- counts[, filtered]
  lst(counts, filtered)
}


cut_unrepresented_milestones <- function(milestone_network, milestone_percentages, milestone_ids) {
  unrepresented_milestones <- setdiff(milestone_ids, milestone_percentages$milestone_id)

  for(milestone_id in unrepresented_milestones) {
    milestone_network_from <- milestone_network %>% filter(from == milestone_id)
    milestone_network_to <- milestone_network %>% filter(to == milestone_id)
    if (nrow(milestone_network_from) > 0 & nrow(milestone_network_to) > 0) {
      milestone_network <- bind_rows(
        milestone_network,
        crossing(milestone_network_from, milestone_network_to) %>%
          mutate(length = length + length1) %>%
          mutate(from = from1) %>%
          select(from, to, length, directed)
      )
    }

    milestone_network <- milestone_network %>%
      filter(from != milestone_id & to != milestone_id)
  }

  milestone_network
}
