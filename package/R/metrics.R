#' Metrics used in the characterisation
#' @export
characterised_metrics <- dyneval::metrics %>%
  filter(metric_id %in% c("correlation", "rf_nmse", "lm_nmse", "edge_flip", "featureimp_cor", "F1_branches", "F1_milestones", "harm_mean"))


#' Metrics used in the evaluation
#' @export
evaluation_metrics <- dyneval::metrics %>%
  filter(metric_id %in% c("correlation", "edge_flip", "featureimp_cor", "F1_branches", "harm_mean"))
