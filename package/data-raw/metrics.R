metrics <- tribble(
  ~metric_id, ~descriptive_name, ~name, ~perfect, ~worst,
  "correlation", "Correlation", "Ordering", 1, 0,
  "rf_mse", "RF MSE", "Neighbourhood", 0, 1,
  "rf_mse_inv", "RF MSE inverted", "Neighbourhood", 1, 0,
  "edge_flip", "Edge flip", "Topology", 1, 0
)

devtools::use_data(metrics, overwrite = TRUE)
