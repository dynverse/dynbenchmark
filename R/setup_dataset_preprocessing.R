#' Helper functions for creating new datasets
#'
#' @param prefix Dataset prefix
#' @param dataset_id Dataset id
#' @param dataset Dataset object to save
#' @param filename filename
#'
#' @export
#'
#' @rdname dataset_preprocessing
dataset_preprocessing <- function(dataset_id) {
  # check whether the working directory is indeed the dynalysis folder
  dynalysis_folder <- get_dynalysis_folder()

  # set option
  options(
    dynalysis_datasetpreproc_id = dataset_id
  )
}

#' @rdname dataset_preprocessing
#' @export
datasetpreproc_getid <- function() {
  dataset_id <- getOption("dynalysis_datasetpreproc_id")
  if (is.null(dataset_id)) {
    stop("No dataset_id found. Did you run dataset_preprocessing(...)?")
  }
  dataset_id
}


# create a helper function
datasetpreproc_subfolder <- function(path) {
  function(filename = "", dataset_id = NULL, relative=FALSE) {
    dyn_fold <- get_dynalysis_folder()
    if(relative) {dyn_fold = ""}

    if (is.null(dataset_id)) {
      dataset_id <- datasetpreproc_getid()
    }

    # determine the full path
    full_path <- paste0(dyn_fold, "/", path, "/", dataset_id, "/")

    # create if necessary
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

    # get complete filename
    paste0(full_path, filename)
  }
}

#' @rdname dataset_preprocessing
#' @export
dataset_preproc_file <- datasetpreproc_subfolder("analysis/data/derived_data/datasets_preproc")

#' @rdname dataset_preprocessing
#' @export
dataset_file <- datasetpreproc_subfolder("analysis/data/derived_data/datasets")

#' @rdname dataset_preprocessing
#' @export
save_dataset <- function(dataset, dataset_id = NULL) {
  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
}

#' Loading a dataset after it has been preprocessed
#' @export
#' @inheritParams dataset_preprocessing
load_dataset <- function(dataset_id = NULL) {
  read_rds(dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
}

#' List the names of all real datasets
#' @export
list_datasets <- function() {
  paste0("real/", list.files(dataset_file(filename = "", dataset_id = "real")))
}

#' Download a file and return its location path
#' @param url The url of the file to download
#' @param filename What name to give to the file
#' @export
download_dataset_file <- function(filename, url, dataset_id = NULL) {
  loc <- dataset_preproc_file(dataset_id = dataset_id, filename = filename)

  if (!file.exists(loc)) {
    download.file(url, loc, method="libcurl")
  }

  loc
}

#' @importFrom dynnormaliser normalise_filter_counts generate_prior_information
#' @export
datasetpreproc_normalise_filter_wrap_and_save <- function(
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
  ) %>% dynnormaliser::add_prior_information_to_wrapper()

  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
  write_rds(original_counts, dataset_file(dataset_id = dataset_id, filename = "original_counts.rds"))

  pdf(dataset_file(dataset_id = dataset_id, "normalisation.pdf"));walk(norm_out$normalisation_plots, print);graphics.off()
}

convert_to_symbol <- function(counts) {
  id_mapper <- readRDS(derived_file("id_mapper.rds", experiment_id = "normalisation"))

  colnames(counts) <- tibble(gene_id = colnames(counts)) %>%
    left_join(id_mapper, by=c("gene_id"="ensembl")) %>%
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


add_bifurcating_intermediate_nodes <- function(milestone_network, milestone_ids) {
  map(milestone_ids, function(milestone_id) {
    milestone_network_from <- milestone_network %>% filter(from == milestone_id)
    milestone_network_to <- milestone_network %>% filter(to == milestone_id)
    if(nrow(milestone_network_to) == 0 && milestone_network_from >= 2) {
      intermediate_node <- paste0("INTERMEDIATE_", dynutils::random_time_string())
      bind_rows(
        tibble(from = milestone_id, to = intermediate_node, length=1e-10, directed=TRUE),
        milestone_network_from %>% mutate(from = intermediate_node)
      )
    } else {
      milestone_network_from
    }
  }) %>% bind_rows()
}
