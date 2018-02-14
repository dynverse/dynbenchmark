#' Helper functions for creating new datasets
#'
#' @param prefix Dataset prefix
#' @param dataset_id Dataset id
#' @param dataset Dataset object to save
#' @param filename filename
#'
#' @importFrom lazyeval lazy_eval
#' @importFrom testthat expect_match
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

  conversion_out <- convert_to_symbol(counts)
  original_counts <- conversion_out$counts
  feature_info <- feature_info[conversion_out$filtered, ] %>% mutate(feature_id = colnames(original_counts))

  norm_out <- normalise_filter_counts(original_counts, verbose = TRUE)

  pdf(dataset_file(dataset_id = dataset_id, "normalisation.pdf"));walk(norm_out$normalisation_plots, print);graphics.off()

  normalisation_info <- norm_out$info

  expression <- norm_out$expression
  counts <- norm_out$counts

  cell_ids <- rownames(counts)
  cell_info <- cell_info %>% slice(match(cell_ids, cell_id))
  cell_grouping <- cell_grouping %>% filter(cell_id %in% cell_ids)
  feature_info <- feature_info %>% slice(match(colnames(counts), feature_id))
  milestone_percentages <- milestone_percentages %>% filter(cell_id %in% cell_ids)

  dataset <- wrap_ti_task_data(
    id = dataset_id,
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info,
    normalisation_info = normalisation_info,
    task_group = "real"
  )

  dataset$prior_information <- dynnormaliser::generate_prior_information(
    milestone_ids,
    milestone_network,
    dataset$progressions,
    milestone_percentages,
    counts,
    feature_info,
    cell_info
  )

  dataset$geodesic_dist <- dynutils::compute_tented_geodesic_distances(dataset)

  dataset$creation_date <- Sys.time()

  write_rds(dataset, dataset_file(dataset_id = dataset_id, filename = "dataset.rds"))
  write_rds(original_counts, dataset_file(dataset_id = dataset_id, filename = "original_counts.rds"))
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
