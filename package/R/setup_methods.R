#' Setup the singularity methods
#'
#' @export
setup_singularity_methods <- function() {
  options(
    dynwrap_singularity_images_folder = derived_file("singularity_images/", experiment_id = "03-method_characterisation"),
    dynwrap_run_environment = "singularity"
  )
}
