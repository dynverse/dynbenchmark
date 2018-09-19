#' Helper function for controlling experiments
#'
#' @param experiment_id id for the experiment
#' @param filename the filename
#' @param remote The remote to access, "" if working locally, TRUE if using default qsub_config remote
#'
#' @export
#'
#' @rdname experiment
#'
#' @examples
#' \dontrun{
#' experiment("test_plots")
#'
#' data <- matrix(runif(200), ncol = 2)
#' pdf(result_file("testplot.pdf"), 5, 5)
#' plot(data)
#' dev.off()
#' }
experiment <- function(experiment_id) {
  # check whether the working directory is indeed the dynbenchmark folder
  dyn_fold <- get_dynbenchmark_folder()

  # set option
  options(dynbenchmark_experiment_id = experiment_id)
}

# create a helper function
experiment_subfolder <- function(path) {
  function(filename = "", experiment_id = NULL, remote = FALSE) {
    filename <- paste0(filename, collapse = "")

    dyn_folder <- get_dynbenchmark_folder(remote = remote)

    # check whether exp_id is given
    if (is.null(experiment_id)) {
      experiment_id <- getOption("dynbenchmark_experiment_id")
    }

    # check whether exp_fold could be found
    if (is.null(experiment_id)) {
      stop("No experiment folder found. Did you run experiment(...) yet?")
    }

    # determine the full path
    full_path <- paste0(dyn_folder, "/", path, "/", experiment_id, "/")

    # create if necessary
    if (is.logical(remote) && !remote) {
      dir.create(full_path, showWarnings = FALSE, recursive = TRUE)
    } else {
      qsub::mkdir_remote(full_path, remote = remote)
    }

    # get complete filename
    paste(full_path, filename, sep = "")
  }
}

#' @rdname experiment
#' @export
derived_file <- experiment_subfolder("derived")

#' @rdname experiment
#' @export
raw_file <- experiment_subfolder("raw")

#' @rdname experiment
#' @export
result_file <- experiment_subfolder("results")

#' @rdname experiment
#' @export
scripts_file <- experiment_subfolder("scripts")
