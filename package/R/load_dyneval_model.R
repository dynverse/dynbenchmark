#' Load model from benchmark suite results
#'
#' @param method_id The short_name of a method
#' @param df A data frame containing the crossing of which models to load
#' @param auto_gc Whether or not to automatically garbage collect after loading models
#' @param experiment_id The experiment id (default NULL
#'
#' @export
load_dyneval_model <- function(method_id, df, auto_gc = TRUE, experiment_id = NULL) {
  models_filename <- derived_file(paste0("suite/", method_id, "/output_models.rds"), experiment_id = experiment_id)
  metrics_filename <- derived_file(paste0("suite/", method_id, "/output_metrics.rds"), experiment_id = experiment_id)

  if (!file.exists(models_filename)) {
    stop("Models of method ", sQuote(method_id), " not found.", sep = "")
  }

  models <- read_rds(models_filename)
  metrics <- read_rds(metrics_filename)

  ix <- metrics %>% mutate(i = row_number()) %>% inner_join(df, by = colnames(df)) %>% pull(i)

  if (length(ix) == 0) {
    stop("df did not match with any of the rows")
  }

  model <- models[ix]

  if (auto_gc) {
    gc()
  }

  model
}
