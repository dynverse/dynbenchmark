#' Common metric information
#' @export
metrics <- tribble(
  ~metric_id, ~descriptive_name, ~name, ~perfect,
  "correlation", "Correlation", "Ordering", 1,
  "rf_mse", "RF MSE", "Neighbourhood", 0,
  "rf_mse_inv", "RF MSE inverted", "Neighbourhood", 1,
  "edge_flip", "Edge flip", "Topology",1
)
