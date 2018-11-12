common_labels <- tribble(
  ~id, ~long, ~short,
  "directed_acyclic_graph", "Directed acyclic graph", "DAG",
  "disconnected_directed_graph", "Disconnected directed graph", "DDG",
  "n_genes", "Number of genes", "# genes",
  "n_cells", "Number of cells", "# cells",
  "ngenes", "Number of genes", "# genes",
  "ncells", "Number of cells", "# cells",
  "n_methods", "Number of methods", "# methods",
  "n_tools", "Number of tools", "# tools",
  "silver", "Silver standard", "Silver",
  "gold", "Gold standard", "Gold",
  "gse", "Source(s)", "Source(s)",
  "gs", "Gold standard", "Gold standard",
  "rf_mse", "Random Forest MSE", "RF MSE",
  "maximal_trajectory_type", "Most complex trajectory type", "Trajectory type",
  "qc_score", "QC score", "QC score",
  "component_id", "Component", "Component",
  "p_value", "p-value", "p-value",
  "q_value", "Adjusted p-value", "p-value (adj.)",
  TRUE, "Yes", "Yes",
  FALSE, "No", "No",
  "trajectory_type_gold", "Gold standard trajectory type", "Gold trajectory type",
  "trajectory_type_predicted", "Predicted trajectory type", "Predicted trajectory type",
  "harm_mean", "Harmonic mean", "Harmonic mean",
  "dataset_source", "Dataset source", "Dataset source",
  "source", "Dataset source", "Dataset source",
  "qc", "Quality Control", "QC",
  "graph", "Connected graph", "Connected graph",
  "offtheshelf", "Off-the-shelf", "Off-the-shelf"
)


labels <- bind_rows(
  common_labels,
  prior_types %>% select(id = prior_id, long = prior_name, short = prior_name)
)

usethis::proj_set("package")
usethis::use_data(labels, overwrite = TRUE)
