#' Common manual labeling
#' @export
labels <- tibble::tribble(
  ~id, ~long, ~short,
  "directed_acyclic_graph", "Directed acyclic graph", "DAG",
  "disconnected_directed_graph", "Disconnected directed graph", "DDG",
  "ngenes", "Number of genes", "# genes",
  "ncells", "Number of cells", "# cells",
  "n_methods", "Number of methods", "# methods",
  "silver", "Silver standard", "Silver",
  "gold", "Gold standard", "Gold",
  "gse", "Source(s)", "Source(s)",
  "gs", "Gold standard", "Gold standard",
  "rf_mse", "Random Forest MSE", "RF MSE",
  "maximal_trajectory_type", "Most complex trajectory type", "Trajectory type",
  "qc_score", "QC score", "QC score"
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

label_wrap <- function(x, width=10) {
  map_chr(strwrap(x, width, simplify=FALSE), paste0, collapse="\n")
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
