#' Helper function for controlling experiments
#'
#' @param dirname Foldername for the experiment
#'
#' @importFrom glue glue
#' @importFrom lazyeval lazy_eval
#' @importFrom testthat expect_match
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
experiment <- function(dirname) {
  # check whether the working directory is indeed the dynalysis folder
  dyn_fold <- get_dynalysis_folder()

  # set option
  options(dynalysis_experiment_folder = dirname)
}

# create a helper function
experiment_subfolder <- function(path) {
  function(...) {
    dyn_fold <- get_dynalysis_folder()
    exp_fold <- getOption("dynalysis_experiment_folder")

    # check whether exp_fold could be found
    if (is.null(exp_fold)) {
      stop("No experiment folder found. Did you run experiment(...) yet?")
    }

    # determine the full path
    full_path <- paste0(dyn_fold, "/", path, "/", dynfold, "/")

    # create if necessary
    dir.create(full_path, recursive = TRUE, showWarnings = FALSE)

    # get complete filename
    paste(full_path, ..., collapse = "", sep = "")
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

