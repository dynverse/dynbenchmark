#' Load model from benchmark suite results
#'
#' @param method_short_name The short_name of a method
#' @param model_id The id of the model to be loaded
#' @param auto_gc Whether or not to automatically garbage collect after loading models
#' @param experiment_id The experiment id (default NULL
#'
#' @export
load_dyneval_model <- function(method_short_name, model_id, auto_gc = TRUE, experiment_id = NULL) {
  models_filename <- derived_file(paste0("suite/", method_short_name, "/output_models.rds"), experiment_id = experiment_id)

  if (!file.exists(models_filename)) {
    stop("Models of method ", sQuote(method_short_name), " not found.", sep = "")
  }

  models <- read_rds(models_filename)

  if (!model_id %in% names(models)) {
    stop("Model ", sQuote(model_id), " of method ", sQuote(method_short_name), " not found.", sep = "")
  }

  model <- models[model_id]

  if (auto_gc) {
    gc()
  }

  model
}
