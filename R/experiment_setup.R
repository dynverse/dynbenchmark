#' Helper function for controlling experiments
#'
#' @param dirname Foldername for the experiment
#' @param description TODO
#' @param auto_create_folders Automatically create folders
#'  when one of the file functions is called.
#' @param ... Strings that will be pasted together to create a filename
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
#' experiment(
#'   dirname = "test_plots",
#'   description = "this parameter does not do anything yet",
#'   auto_create_folders = TRUE,
#'   {
#'     data <- matrix(runif(200), ncol = 2)
#'     pdf(figure_file("testplot.pdf"), 5, 5)
#'     plot(data)
#'     dev.off()
#'   }
#' )
#' }
experiment <- function(dirname, description, auto_create_folders = TRUE) {
  # check whether the working directory is indeed the dynalysis folder
  testthat::expect_match(getwd(), "/dynalysis/?$")

  # set option
  options(dynalysis_experiment_folder = dirname)
  options(dynalysis_experiment_autocreate = TRUE)
}

# create a helper function
auto_create_fun <- function(path) {
  function(...) {
    dynfold <- getOption("dynalysis_experiment_folder")
    if (is.null(dynfold)) {
      stop("No experiment folder found. Did you run experiment(...) yet?")
    }
    full_path <- paste0(path, "/", dynfold, "/")
    if (getOption("dynalysis_experiment_autocreate") && !file.exists(full_path)) {
      dir.create(full_path, recursive = TRUE)
    }
    paste(full_path, ..., collapse = "", sep = "")
  }
}

#' @rdname experiment
#' @export
scratch_file <- auto_create_fun("analysis/data/derived_data")

#' @rdname experiment
#' @export
raw_file <- auto_create_fun("analysis/data/raw_data")

#' @rdname experiment
#' @export
figure_file <- auto_create_fun("analysis/figures")

#' @rdname experiment
#' @export
result_file <- auto_create_fun("analysis/results")

