#' Non inclusion reasons
#' @export
non_inclusion_reasons <- tribble(
  ~id, ~long,
  "not_free", "Not free",
  "unavailable", "Unavailable",
  "superseded", "Superseded",
  "not_expression_based", "Requires data types other than expression",
  "gui_only", "No programming interface",
  "unwrappable", "Unresolved errors during wrapping",
  "speed", "Slow",
  "date", "Too late to be included in current version of the evaluation",
  "no_ordering", "Doesn't return an ordering"
) %>% mutate(footnote = seq_along(id))
