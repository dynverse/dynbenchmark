#' Find the dynbenchmark path
#'
#' When executing some of the dynbenchmark functions, the git repository needs to be found.
#' Either set the working directory to the repository, or define the DYNBENCHMARK_PATH variable.
#'
#' @param remote The remote to access, "" if running locally, TRUE if using default qsub config remote
#'
#' @export
get_dynbenchmark_folder <- function(remote = FALSE) {
  if (is.logical(remote) && !remote) {
    pwd <- getOption("dynbenchmark_path")

    if (is.null(pwd)) {
      pwd <- Sys.getenv("DYNBENCHMARK_PATH")
    }

    if (is.null(pwd)) {
      pwd <- getwd()
    }

    if (pwd == "") {
      pwd <- paste0("./")
    }

    if (!file.exists(paste0(pwd, "dynbenchmark.Rproj"))) {
      stop("dynbenchmark folder could not be found. Either set it as the working directory, or export a DYNBENCHMARK_PATH variable.")
    }

    pwd
  } else {
    pwd <- qsub::run_remote("echo $DYNBENCHMARK_PATH", remote)$stdout

    if (pwd == "") {
      stop("DYNBENCHMARK_PATH not found at remote")
    }

    pwd
  }
}
