#' Helper functions for creating new datasets
#'
#' @param prefix Dataset prefix
#' @param dataset_id Dataset id
#' @param dataset Dataset object to save
#' @param ... filenames
#'
#' @importFrom lazyeval lazy_eval
#' @importFrom testthat expect_match
#' @export
#'
#' @rdname dataset_preprocessing
dataset_preprocessing <- function(prefix, dataset_id) {
  # check whether the working directory is indeed the dynalysis folder
  dynalysis_folder <- get_dynalysis_folder()

  # set option
  options(
    dynalysis_datasetpreproc_prefix = prefix,
    dynalysis_datasetpreproc_id = dataset_id
  )
}

#' @rdname dataset_preprocessing
#' @export
datasetpreproc_getprefix <- function() {
  prefix <- getOption("dynalysis_datasetpreproc_prefix")
  if (is.null(prefix)) {
    stop("No prefix found. Did you run dataset_preprocessing(...)?")
  }
  prefix
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
  function(..., prefix = NULL, dataset_id = NULL) {
    dyn_fold <- get_dynalysis_folder()

    if (is.null(prefix)) {
      prefix <- datasetpreproc_getprefix()
    }
    if (is.null(dataset_id)) {
      dataset_id <- datasetpreproc_getid()
    }

    # determine the full path
    full_path <- paste0(dyn_fold, "/", path, "/", prefix, "/", dataset_id, "/")

    # create if necessary
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

    # get complete filename
    paste(full_path, ..., collapse = "", sep = "")
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
save_dataset <- function(dataset, prefix = NULL, dataset_id = NULL) {
  write_rds(dataset, dataset_file("dataset.rds", prefix = prefix, dataset_id = dataset_id))
}

#' Loading a dataset after it has been preprocessed
#' @export
#' @inheritParams dataset_preprocessing
load_dataset <- function(prefix, dataset_id) {
  read_rds(dataset_file("dataset.rds", prefix=prefix, dataset_id=dataset_id))
}

#' Download a file and return its location path
#' @param url The url of the file to download
#' @param filename What name to give to the file
#' @export
download_dataset_file <- function(url, filename) {
  loc <- dataset_preproc_file(filename)

  if (!file.exists(loc)) {
    download.file(url, loc, method="libcurl")
  }

  loc
}

#' @export
datasetpreproc_normalise_filter_wrap_and_save <- function(
  dataset_prefix,
  dataset_id,
  ti_type,
  counts,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages,
  cell_grouping,
  cell_info,
  feature_info
) {
  counts <- convert_to_symbol(counts)

  norm_out <- normalize_filter_counts(counts, verbose=TRUE)

  pdf(dataset_file("normalization.pdf"));walk(norm_out$normalization_plots, print);graphics.off()

  expression <- norm_out$expression
  counts <- norm_out$counts

  cell_ids <- rownames(counts)
  cell_info <- cell_info %>% slice(match(cell_ids, cell_id))
  cell_grouping <- cell_grouping %>% filter(cell_id %in% cell_ids)
  feature_info <- feature_info %>% slice(match(colnames(counts), feature_id))
  milestone_percentages <- milestone_percentages %>% filter(cell_id %in% cell_ids)

  dataset <- wrap_ti_task_data(
    ti_type = ti_type,
    id = dataset_id,
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )

  save_dataset(dataset, dataset_prefix, dataset_id)
}

convert_to_symbol <- function(counts) {
  id_mapper <- readRDS(derived_file("id_mapper.rds", experiment_id = "normalization"))

  colnames(counts) <- tibble(gene_id = colnames(counts)) %>%
    left_join(id_mapper, by=c("gene_id"="ensembl")) %>%
    mutate(gene_id = ifelse(is.na(symbol), gene_id, symbol)) %>%
    pull(gene_id)

  counts
}
