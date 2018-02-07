#' Non inclusion reasons
#' @export
non_inclusion_reasons <- tribble(
  ~id, ~long,
  "not_free", "Not free",
  "unavailable", "Unavailable",
  "superseded", "Superseded by another method",
  "not_expression_based", "Requires data types other than expression",
  "gui_only", "No programming interface",
  "unwrappable", "Unresolved errors during wrapping",
  "speed", "Too slow (requires more than one hour on a 100x100 dataset)",
  "no_ordering", "Doesn't return an ordering",
  "user_input", "Requires additional user input during the algorithm (not prior information)",
  "date", "Published later than 2017-05-01 to be included in the current version of the evaluation"
) %>% mutate(footnote = seq_along(id))
