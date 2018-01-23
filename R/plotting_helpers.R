#' Common manual labeling
#' @export
labels <- tibble::tribble(
  ~id, ~long, ~short,
  "directed_acyclic_graph", "Directed acyclic graph", "DAG",
  "ngenes", "Number of genes", "# genes",
  "ncells", "Number of cells", "# cells",
  "silver", "Silver standard", "Silver standard",
  "gold", "Gold standard", "Gold standard"
)

#' Short labelling function
#' @param x The text!
#' @export
label_short <- function(x) {
  tibble(id = as.character(x)) %>%
    left_join(labels, "id") %>%
    mutate(short=ifelse(is.na(short), label_capitalise(id), short)) %>%
    pull(short)
}

#' Long labelling function
#' @param x The text!
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
