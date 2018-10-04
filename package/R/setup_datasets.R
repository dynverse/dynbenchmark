#' Helper functions for creating new datasets
#'
#' @param id The ID of the dataset to be used
#' @param dataset Dataset object to save
#' @param filename Custom filename
#' @inheritParams experiment
#' @param lazy_load Whether or not to allow for lazy loading of large objects in the dataset
#'
#' @export
#'
#' @rdname dataset_preprocessing
dataset_preprocessing <- function(id) {
  # check whether the working directory is indeed the dynbenchmark folder
  dynbenchmark_folder <- get_dynbenchmark_folder()

  # set option
  options(
    dynbenchmark_datasetpreproc_id = id
  )
}

#' @rdname dataset_preprocessing
#' @export
get_dataset_preprocessing_id <- function() {
  id <- getOption("dynbenchmark_datasetpreproc_id")
  if (is.null(id)) {
    stop("No id found. Did you run dataset_preprocessing(...)?")
  }
  id
}

# create a helper function
#' @include setup_experiment.R
datasetpreproc_subfolder <- function(path) {
  function(filename = "", id = NULL, remote = FALSE) {
    if (is.null(id)) {
      id <- get_dataset_preprocessing_id()
    }

    experiment_subfolder(path)(filename = filename, experiment_id = id, remote = remote)
  }
}

#' @rdname dataset_preprocessing
#' @export
dataset_source_file <- datasetpreproc_subfolder("derived/01-datasets_preproc/source")

#' Download a file and return its location path
#' @param url The url of the file to download
#' @param filename What name to give to the file
#' @param id An optional id
#' @export
#' @importFrom utils download.file
download_dataset_source_file <- function(filename, url, id = NULL) {
  loc <- dataset_source_file(id = id, filename = filename)

  if (!file.exists(loc)) {
    utils::download.file(url, loc, method = "libcurl")
  }

  loc
}

#' @rdname dataset_preprocessing
#' @export
dataset_raw_file <- function(id) {
  file <- derived_file(paste0(id, ".rds"), experiment_id = "01-datasets_preproc/raw")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  file
}

#' @rdname dataset_preprocessing
#' @export
save_raw_dataset <- function(dataset, id = get_dataset_preprocessing_id()) {
  dataset$id <- id

  write_rds(dataset, dataset_raw_file(id))
}

#' @rdname dataset_preprocessing
#' @export
dataset_file <- datasetpreproc_subfolder("derived/01-datasets")

#' @rdname dataset_preprocessing
#' @export
save_dataset <- function(dataset, id = NULL, lazy_load = TRUE) {
  dir.create(dataset_file(filename = "", id = id), showWarnings = FALSE)

  if (lazy_load) {
    for (col in c("expression", "counts")) {
      if (!is.function(dataset[[col]])) {
        col_file <- dataset_file(filename = paste0(col, ".rds"), id = id)
        write_rds(dataset[[col]], col_file, compress = "xz")

        env <- new.env(baseenv())
        assign("id", id, env)
        assign("col", col, env)
        dataset[[col]] <- function() {
          readr::read_rds(dynbenchmark::dataset_file(paste0(col, ".rds"), id = id))
        }
        environment(dataset[[col]]) <- env
      }
    }
  }

  write_rds(dataset, dataset_file(filename = "dataset.rds", id = id), compress = "xz")
}

#' @rdname load_dataset
#' @param prefix The prefix, eg. real or synthetic
#' @export
list_datasets <- function(prefix = "") {
  ids <- list.files(
    derived_file("", experiment_id = "01-datasets"),
    "dataset\\.rds",
    recursive = TRUE
  ) %>%
    dirname() %>%
    str_subset(paste0("^", prefix))

  tibble(
    id = ids,
    source = gsub("(.*)/[^/]*", "\\1", ids),
    name = gsub(".*/([^/]*)", "\\1", ids)
  )
}

#' Load datasets
#' @export
#' @inheritParams dataset_preprocessing
#' @param as_tibble Return the datasets as a tibble or as a list of datasets?
#'
#' @rdname load_dataset
load_dataset <- function(id, as_tibble = FALSE) {
  dataset <- read_rds(dataset_file(filename = "dataset.rds", id = id))

  if (as_tibble) {
    dataset <- list_as_tibble(list(dataset))

    if ("date" %in% colnames(dataset)) {
      dataset$date <- as.Date(dataset$date, origin = "1970-01-01")
    }
    if ("creation_date" %in% colnames(dataset)) {
      dataset$creation_date = as.POSIXct(dataset$creation_date, origin = "1970-01-01")
    }
  }

  dataset
}

#' @export
#' @param ids Character vector of dataset identifiers
#' @rdname load_dataset
load_datasets <- function(ids = list_datasets()$id, as_tibble = TRUE) {
  testthat::expect_true(is.character(ids))

  datasets <- map(ids, load_dataset, as_tibble = as_tibble)

  if (as_tibble) {
    bind_rows(datasets)
  } else {
    datasets
  }
}
