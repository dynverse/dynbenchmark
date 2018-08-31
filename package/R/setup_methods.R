#' Setup the singularity methods
#'
#' @export
setup_singularity_methods <- function() {
  options(
    dynwrap_singularity_images_folder = derived_file("", experiment_id = "singularity_images"),
    dynwrap_run_environment = "singularity"
  )
}
