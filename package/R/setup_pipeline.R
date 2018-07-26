#' Load or generate an R object
#' @param file_location Location of the RDS file
#' @param expression Expression to run if the RDS file is not found
load_or_generate <- function(file_location, expression) {
  if (file.exists(file_location)) {
    message("Loaded ", basename(file_location))
    x <- read_rds(file_location)
  } else {
    message("Creating ", basename(file_location))
    x <- eval(expression)
    dir.create(dirname(file_location), showWarnings = FALSE, recursive = TRUE)
    write_rds(x, file_location)
  }
  x
}
