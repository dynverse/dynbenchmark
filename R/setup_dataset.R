#' Helper function for creating new datasets
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

# create a helper function
datasetpreproc_subfolder <- function(path) {
  function(..., prefix = NULL, dataset_id = NULL) {
    dyn_fold <- get_dynalysis_folder()

    if (is.null(prefix)) {
      prefix <- getOption("dynalysis_datasetpreproc_prefix")
    }
    if (is.null(dataset_id)) {
      dataset_id <- getOption("dynalysis_datasetpreproc_id")
    }

    # check whether exp_fold could be found
    if (is.null(prefix) || is.null(dataset_id)) {
      stop("No dataset folder found. Did you run dataset_preprocessing(...) yet?")
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
  write_rds(dataset, dataset_file("dataset.rds", prefix = NULL, dataset_id = dataset_id))
}

#' Loading a dataset after it has been preprocessed
#' @export
#' @inheritParams dataset_preprocessing
load_dataset <- function(prefix, dataset_id) {
  read_rds(dataset_file("dataset.rds", prefix, dataset_id))
}
