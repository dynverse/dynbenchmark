#' Helper function for controlling experiments
#'
#' @param experiment_id id for the experiment
#' @param filename the filename
#' @param glue if specified, glue::glue this string and override filename.
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
#' pdf(figure_file("testplot.pdf"), 5, 5)
#' plot(data)
#' dev.off()
#' }
experiment <- function(experiment_id) {
  # check whether the working directory is indeed the dynalysis folder
  dyn_fold <- get_dynalysis_folder()

  # set option
  options(dynalysis_experiment_id = experiment_id)
}

# create a helper function
#' @importFrom glue glue
experiment_subfolder <- function(path) {
  function(filename= "", experiment_id = NULL) {
    filename <- paste0(filename, collapse = "")

    dyn_fold <- get_dynalysis_folder()

    # check whether exp_id is given
    if (is.null(experiment_id)) {
      experiment_id <- getOption("dynalysis_experiment_id")
    }

    # check whether exp_fold could be found
    if (is.null(experiment_id)) {
      stop("No experiment folder found. Did you run experiment(...) yet?")
    }

    # determine the full path
    full_path <- paste0(dyn_fold, "/", path, "/", experiment_id, "/")

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
    paste(full_path, filename, sep = "")
  }
}

#' @rdname experiment
#' @export
derived_file <- experiment_subfolder("analysis/data/derived_data")

#' @rdname experiment
#' @export
raw_file <- experiment_subfolder("analysis/data/raw_data")

#' @rdname experiment
#' @export
figure_file <- experiment_subfolder("analysis/figures")

#' @rdname experiment
#' @export
result_file <- experiment_subfolder("analysis/results")

