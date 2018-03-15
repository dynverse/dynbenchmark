#' Find the dynalysis path
#'
#' When executing some of the dynalysis functions, the git repository needs to be found.
#' Either set the working directory to the repository, or define the DYNALYSIS_PATH variable.
#'
#' @export
#' @importFrom dynutils pritt
get_dynalysis_folder <- function() {
  pwd <- Sys.getenv("DYNALYSIS_PATH")

  if (pwd == "") {
    pwd <- getwd()
  }

  if (!file.exists(pritt("{pwd}/dynalysis.Rproj"))) {
    if(exists(".dynalysis_path")) {
      pwd <- .dynalysis_path
    } else {
      stop("dynalysis folder could not be found. Either set it as the working directory, or export a DYNALYSIS_PATH variable.")
    }
  }

  pwd
}
