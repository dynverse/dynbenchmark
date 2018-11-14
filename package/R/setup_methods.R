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
#' @param default_filter Default filtering for the benchmark.
#' @param ... Other filters for the method tibble.
#'
#' @export
load_methods <- function(
  default_filter = source == "tool" | id %in% c("angle", "comp1", "mst"),
  ...
) {
  read_rds(result_file("methods.rds", "03-methods")) %>%
    filter(!!!rlang::enquos(default_filter, ...))
}
