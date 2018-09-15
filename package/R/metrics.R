#' Metrics used in the characterisation
#' @export
metrics_characterised <- dplyr::filter(
  dyneval::metrics,
  !metric_id %in% c("featureimp_ks", "featureimp_wilcox", "rf_rsq", "lm_rsq", "rf_mse", "lm_mse")
)


#' Metrics used in the evaluation
#' @export
metrics_characterised <- dplyr::filter(
  dyneval::metrics,
  metric_id %in% c("correlation", "him", "featureimp_wcor", "F1_branches", "geom_mean")
)
