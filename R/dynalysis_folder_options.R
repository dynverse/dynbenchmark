#' Find the dynalysis path
#'
#' When executing some of the dynalysis functions, the git repository needs to be found.
#' Either set the working directory to the repository, or define the DYNALYSIS_PATH variable.
#'
#' @export
get_dynalysis_folder <- function() {
  pwd <- Sys.getenv("DYNALYSIS_PATH")

  if (pwd == "") {
    pwd <- getwd()
  }

  if (!file.exists(pritt("{pwd}/dynalysis.Rproj"))) {
    stop("dynalysis folder could not be found. Either set it as the working directory, or export a DYNALYSIS_PATH variable.")
  }

  pwd
}
