#' #' @export
#' load_dataset <- function(dataset_id, prefix="") {
#'   if(file.exists("/group/irc/shared")) {
#'     read_rds(paste0("/group/irc/shared/dynalysis/analysis/data/datasets/", prefix, "/", dataset_id, ".rds"))
#'   } else {
#'     read_rds(paste0("analysis/data/datasets/", prefix, "/", dataset_id, ".rds"))
#'   }
#' }
#'
#' #' @export
#' save_dataset <- function(dataset) {
#'   write_rds(dataset, paste0("analysis/data/datasets/real/", dataset$info$id, ".rds"))
#' }


#' Helper function for creating new datasets
#'
#' @param prefix Dataset prefix
#' @param dataset_id Dataset id
#'
#' @importFrom glue glue
#' @importFrom lazyeval lazy_eval
#' @importFrom testthat expect_match
#' @export
#'
#' @rdname dataset_preproccessing
dataset_preproccessing <- function(prefix, dataset_id) {
  # check whether the working directory is indeed the dynalysis folder
  dynalysis_folder <- get_dynalysis_folder()

  # set option
  options(
    dynalysis_datasetpreproc_id = dataset_id,
    dynalysis_datasetpreproc_prefix = prefix
  )
}

# create a helper function
datasetpreproc_subfolder <- function(path) {
  function(...) {
    dyn_fold <- get_dynalysis_folder()
    prefix <- getOption("dynalysis_datasetpreproc_id")
    dataset_id <- getOption("dynalysis_datasetpreproc_prefix")

    # check whether exp_fold could be found
    if (is.null(dataset_id)) {
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

#' @rdname dataset_preproccessing
#' @export
dataset_preproc_file <- datasetpreproc_subfolder("analysis/data/derived_data/datasets_preproc")

#' @rdname dataset_preproccessing
#' @export
dataset_file <- datasetpreproc_subfolder("analysis/data/datasets")

#' @export
load_dataset <- function(prefix, dataset_id) {
  dyn_fold <- get_dynalysis_folder()
  file <- pritt("{dyn_fold}/analysis/data/datasets/{prefix}/{dataset_id}/dataset.rds")
  read_rds(file)
}

#' @export
save_dataset <- function(dataset) {
  write_rds(dataset, dataset_file("dataset.rds"))
}
