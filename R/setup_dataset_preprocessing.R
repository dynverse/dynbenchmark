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
  read_rds(dataset_file("dataset.rds", prefix, dataset_id))
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
