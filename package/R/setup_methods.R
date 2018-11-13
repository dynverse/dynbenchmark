#' Setup the singularity methods
#'
#' @export
setup_singularity_methods <- function() {
  options(
    dynwrap_singularity_images_folder = derived_file("", experiment_id = "singularity_images"),
    dynwrap_run_environment = "singularity"
  )
}


#' Load the information on the methods
#'
#' @param filter_fun A function with which to filter the methods tibble.
#'
#' @export
load_methods <- function(
  filter_fun = function(tib) tib %>% filter(source == "tool" | id %in% c("angle", "comp1 ", "mst"))
) {
  tib <- read_rds(result_file("methods.rds", "03-methods"))

  if (!is.null(filter_fun)) tib <- tib %>% filter_fun()

  tib
}
