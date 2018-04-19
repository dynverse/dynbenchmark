prior_types <- tribble(
  ~prior_id, ~prior_name,
  "start_id", "Start cell",
  "end_id", "End cell(s)",
  "end_n", "# end states",
  "states_id", "Cell clustering",
  "states_n", "# states",
  "states_network", "State network",
  "time_id", "Time course",
  "genes_id", "Marker genes"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(prior_types, overwrite = TRUE)
devtools::use_data(prior_usages, overwrite = TRUE)
