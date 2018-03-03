#' Common manual labeling
#' @export
labels <- tibble::tribble(
  ~id, ~long, ~short,
  "directed_acyclic_graph", "Directed acyclic graph", "DAG",
  "disconnected_directed_graph", "Disconnected directed graph", "DDG",
  "ngenes", "Number of genes", "# genes",
  "ncells", "Number of cells", "# cells",
  "n_methods", "Number of methods", "# methods",
  "n_implementations", "Number of methods", "# methods",
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
  "trajectory_type_predicted", "Predicted trajectory type", "Predicted trajectory type"
)

#' Short labelling function
#' @param x The text
#' @param width The width of the label
#' @export
label_short <- function(x, width=10) {
  tibble(id = as.character(x)) %>%
    left_join(labels, "id") %>%
    mutate(short=ifelse(is.na(short), label_capitalise(id), short)) %>%
    mutate(short=label_wrap(short, width=width)) %>%
    pull(short)
}

#' @export
label_wrap <- function(x, width=10, collapse="\n") {
  map_chr(strwrap(x, width, simplify=FALSE), paste0, collapse=collapse)
}

#' Long labelling function
#' @param x The text
#' @export
label_long <- function(x) {
  tibble(id = as.character(x)) %>%
    left_join(labels, "id") %>%
    mutate(long=ifelse(is.na(long), label_capitalise(id), long)) %>%
    pull(long)
}

label_capitalise <- function(x) {
  x %>% gsub("_", " ", .) %>% Hmisc::capitalize()
}


#' Labeller for facets
#' @param label_func Which function to use for facet labelling
#' @export
label_facet <- function(label_func = label_long) {function(df) {mutate_all(df, label_func)}}

#' Label trajectory types simplified
#' @param trajectory_types_oi Trajectory types
#' @export
label_simple_trajectory_types <- function(trajectory_types_oi) {
  ifelse(
    trajectory_types_oi %in% trajectory_types$id,
    trajectory_types$simplified[match(trajectory_types_oi, trajectory_types$id)] %>% label_long(),
    label_long(trajectory_types_oi)
  )
}


#' Labels only the extrema (and zero)
#' @param x The values
label_extrema <- function(x) {
  ifelse(x %in% c(0, min(x), max(x)), x, "")
}


#' Label p_value
#' @param p_values P values
#' @param cutoffs Cutoffs for number of asterisks
label_pvalue <- function(p_values, cutoffs=c(0.1, 0.01, 1e-5)) {
  breaks <- c(Inf, cutoffs, -Inf)
  labels <- rev(c("NS", map_chr(seq_len(length(cutoffs)), ~glue::collapse(rep("*", .)))))
  p_values %>% cut(breaks, labels) %>% as.character()
}

