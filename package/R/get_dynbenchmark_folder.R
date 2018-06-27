#' Find the dynbenchmark path
#'
#' When executing some of the dynbenchmark functions, the git repository needs to be found.
#' Either set the working directory to the repository, or define the DYNBENCHMARK_PATH variable.
#'
#' @export
get_dynbenchmark_folder <- function() {
  pwd <- Sys.getenv("DYNBENCHMARK_PATH")

  if (pwd == "") {
    pwd <- NULL
  }

  if (is.null(pwd)) {
    pwd <- getOption("dynbenchmark_path")
  }

  if (is.null(pwd)) {
    pwd <- getwd()
  }

  if (!file.exists(paste0(pwd, "/dynbenchmark.Rproj"))) {
    stop("dynbenchmark folder could not be found. Either set it as the working directory, or export a DYNBENCHMARK_PATH variable.")
  }

  pwd
}
