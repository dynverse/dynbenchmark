non_inclusion_reasons <- tribble(
  ~id, ~long,
  "not_free", "Not freely available",
  "unavailable", "No code available",
  "superseded", "Superseded by another method",
  "not_expression_based", "Requires data types other than expression",
  "gui_only", "No programming interface",
  "unwrappable", "Unresolved errors during wrapping",
  "speed", "Too slow (requires more than one hour on a 100x100 dataset)",
  "no_ordering", "Doesn't return an ordering",
  "user_input", "Requires additional user input during the algorithm (not prior information)"
) %>% mutate(footnote = letters[seq_along(id)])

devtools::use_data(non_inclusion_reasons, overwrite = TRUE)
