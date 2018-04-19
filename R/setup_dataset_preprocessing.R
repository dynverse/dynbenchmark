#' Helper functions for creating new datasets
#'
#' @param dataset_id The ID of the dataset to be used
#' @param dataset Dataset object to save
#' @param filename Custom filename
#' @param glue if specified, glue::glue this string and override filename.
#' @param relative Whether or not to output relative paths
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
#' @importFrom glue glue
datasetpreproc_subfolder <- function(path, ext) {
  use_folder <- ext == "/"
  fun <- function(filename = "", dataset_id = NULL, glue = NULL, relative = FALSE) {
    dyn_fold <- get_dynalysis_folder()

    if (relative) {
      dyn_fold = ""
    }

    if (is.null(dataset_id)) {
      dataset_id <- datasetpreproc_getid()
    }

    # determine the full path
    if (use_folder) {
      full_path <- paste0(dyn_fold, "/", path, "/", dataset_id, "/")
    } else {
      full_path <- paste0(dyn_fold, "/", path, "/")
    }

    # create if necessary
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

    # process glue, if necessary
    if (!is.null(glue)) {
      if (filename != "") {
        stop("if 'glue' is specified, 'filename' must be \"\".")
      }
      filename <- glue::glue(glue)
    }

    # get complete filename
    if (use_folder) {
      paste0(full_path, filename)
    } else {
      paste0(full_path, dataset_id, ext)
    }
  }
  if (!use_folder) {
    formals(fun) <- formals(fun)[-1]
  }
  fun
}

#' @rdname dataset_preprocessing
#' @export
dataset_preproc_file <- datasetpreproc_subfolder("analysis/data/derived_data/1-datasets_preproc", ext = "/")

#' @rdname dataset_preprocessing
#' @export
dataset_file <- datasetpreproc_subfolder("analysis/data/derived_data/1-datasets", ext = ".rds")

#' @rdname dataset_preprocessing
#' @export
save_dataset <- function(dataset, dataset_id = NULL) {
  write_rds(dataset, dataset_file(dataset_id = dataset_id))
}

#' Load a dataset after it has been preprocessed
#' @export
#' @inheritParams dataset_preprocessing
load_dataset <- function(dataset_id = NULL) {
  read_rds(dataset_file(dataset_id = dataset_id))
}

#' Load the tibble of datasets
#' @export
#' @inheritParams dataset_preprocessing
load_datasets_tibble <- function() {
  read_rds(derived_file("tasks.rds", "1-datasets"))
}

#' List the names of all present datasets
#' @export
list_datasets <- function() {
  load_datasets_tibble()$id
}

#' Download a file and return its location path
#' @param url The url of the file to download
#' @param filename What name to give to the file
#' @param dataset_id An optional dataset_id
#' @export
download_dataset_file <- function(filename, url, dataset_id = NULL) {
  loc <- dataset_preproc_file(dataset_id = dataset_id, filename = filename)

  if (!file.exists(loc)) {
    download.file(url, loc, method="libcurl")
  }

  loc
}
