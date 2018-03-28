#' Common metric information
#' @export
metrics <- tribble(
  ~metric_id, ~descriptive_name, ~name,
  "correlation", "Correlation", "Ordering",
  "rf_mse", "RF MSE", "Neighbourhood",
  "edge_flip", "Edge flip", "Topology"
)
