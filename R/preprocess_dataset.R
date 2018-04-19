#' Preprocessing functionality for real datasets
#'
#' @inheritParams dynwrap::wrap_data
#' @inheritParams dynwrap::add_expression_to_wrapper
#' @inheritParams dynwrap::add_trajectory_to_wrapper
#' @param cell_grouping Milestone groups of the cells.
#' @param dataset_id The name of the dataset.
#'
#' @importFrom dynnormaliser normalise_filter_counts
#' @importFrom dynwrap generate_prior_information
#' @export
preprocess_dataset <- function(
  counts,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages,
  cell_grouping,
  cell_info,
  feature_info,
  dataset_id = NULL
) {
  if (is.null(dataset_id)) {
    dataset_id <- datasetpreproc_getid()
  }

  readr::write_lines(as.character(Sys.time()), dataset_file(dataset_id = dataset_id, filename = "date.txt"))

  # convert symbols
  conversion_out <- convert_to_symbol(counts)
  original_counts <- conversion_out$counts
  feature_info <- feature_info[conversion_out$filtered, ] %>% mutate(feature_id = colnames(original_counts))

  # normalise and filter expression
  norm_out <- dynnormaliser::normalise_filter_counts(original_counts, verbose = TRUE)

  normalisation_info <- norm_out$info

  expression <- norm_out$expression
  counts <- norm_out$counts

  cell_ids <- rownames(counts)
  cell_info <- cell_info %>% slice(match(cell_ids, cell_id))
  cell_grouping <- cell_grouping %>% filter(cell_id %in% cell_ids)
  feature_info <- feature_info %>% slice(match(colnames(counts), feature_id))
  milestone_percentages <- milestone_percentages %>% filter(cell_id %in% cell_ids)

  # cut out unrepresented milestones
  milestone_network <- cut_unrepresented_milestones(milestone_network, milestone_percentages, milestone_ids)
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  # add intermediate nodes to bifurcating regions
  milestone_network <- add_bifurcating_intermediate_nodes(milestone_network, milestone_ids)
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  # get progressions
  progressions <- dynwrap::convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages)

  # extract divergence regions
  part1 <- progressions %>%
    group_by(cell_id) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(from, to) %>%
    distinct()

  if (nrow(part1) > 0) {
    divergence_regions <-
      part1 %>%
      group_by(from) %>%
      do({
        data_frame(
          divergence_id = paste0("div_", .$from[[1]]),
          milestone_id = c(.$from[[1]], .$to),
          is_start = c(T, rep(F, nrow(.)))
        )
      }) %>%
      ungroup() %>%
      select(divergence_id, milestone_id, is_start)
  } else {
    divergence_regions <- NULL
  }

  dataset <- dynwrap::wrap_data(
    id = dataset_id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    task_source = "real",
    cell_grouping = cell_grouping,
    normalisation_info = normalisation_info,
    creation_date = Sys.time()
  ) %>% dynwrap::add_trajectory_to_wrapper(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    progressions = progressions
  ) %>% dynwrap::add_expression_to_wrapper(
    counts = counts,
    expression = expression,
    feature_info = feature_info
  ) %>% dynwrap::add_prior_information_to_wrapper()

  save_dataset(dataset, dataset_id = dataset_id)
  write_rds(original_counts, dataset_file(dataset_id = dataset_id, filename = "original_counts.rds"))

  pdf(dataset_file(dataset_id = dataset_id, "normalisation.pdf"))
  walk(norm_out$normalisation_plots, print)
  graphics.off()

  return(invisible())
}

convert_to_symbol <- function(counts) {
  colnames(counts) <- tibble(gene_id = colnames(counts)) %>%
    left_join(id_mapper, by = c("gene_id" = "ensembl")) %>%
    mutate(gene_id = ifelse(is.na(symbol), gene_id, symbol)) %>%
    pull(gene_id)
  filtered <- names(which(table(colnames(counts)) == 1))
  counts <- counts[, filtered] # remove duplicates
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


