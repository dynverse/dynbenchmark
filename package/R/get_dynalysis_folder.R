#' Find the dynbenchmark path
#'
#' When executing some of the dynbenchmark functions, the git repository needs to be found.
#' Either set the working directory to the repository, or define the DYNALYSIS_PATH variable.
#'
#' @export
#' @importFrom dynutils pritt
get_dynbenchmark_folder <- function() {
  pwd <- Sys.getenv("DYNALYSIS_PATH")

  if (pwd == "") {
    pwd <- getwd()
  }

  if (!file.exists(pritt("{pwd}/dynbenchmark.Rproj"))) {
    if(exists(".dynbenchmark_path")) {
      pwd <- .dynbenchmark_path
    } else {
      stop("dynbenchmark folder could not be found. Either set it as the working directory, or export a DYNALYSIS_PATH variable.")
    }
  }

  pwd
}
