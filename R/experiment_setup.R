#' Helper function for controlling experiments
#'
#' @param dirname Foldername for the experiment
#' @param description TODO
#' @param auto_create_folders Automatically create folders
#'  when one of the file functions is called.
#'
#' @importFrom glue glue
#' @importFrom lazyeval lazy_eval
#' @importFrom testthat expect_match
#' @export
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

  # get environment
  env <- parent.env(environment())

  # create a few directories in which to store possible output
  scratch_dir <- glue::glue("analysis/data/derived_data/{dirname}/")
  figure_dir <- glue::glue("analysis/figures/{dirname}/")
  result_dir <- glue::glue("analysis/results/{dirname}/")

  # create a helper function
  auto_create_fun <- function(path) {
    function(...) {
      if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
      paste(path, ..., collapse = "")
    }
  }
  if (auto_create_folders) dir.create(c(scratch_dir, figure_dir, result_dir))

  # create and expose functions
  env$scratch_file <- auto_create_fun(scratch_dir)
  env$figure_file <- auto_create_fun(figure_dir)
  env$result_file <- auto_create_fun(result_dir)
}

