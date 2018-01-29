## Trajectory types -----------------------------------------
# create the DAG of trajectory types

## QC categories -----------------------------------------
applications <- c("developer_friendly", "user_friendly", "good_science")
application_labels <- setNames(applications %>% gsub("_", " ", .) %>% Hmisc::capitalize(), applications)

categories <- c("availability", "code_quality", "code_assurance", "documentation", "behaviour", "paper")
category_colors <- c("#3498DB", "#E74C3C", "#A4CC2E", "#FEB308", "#B10DC9", "#85144b", "#EA8F10", "#2ECC49", "#CC2E63") %>% .[1:length(categories)] %>% setNames(categories)
category_labels <- setNames(categories %>% gsub("_", " ", .) %>% Hmisc::capitalize(), categories)

category_gradients_white <- map(category_colors, function(color) {
  n <- 1000
  ramp <- shades::gradient(c("white", color), n)

  function(x, min=0, max=1) {
    x[x == 0] <- 0.0000000001
    ramp[round((x - min)/(max-min)*(n-1))+1]
  }
})

## Labels -----------------------
# global_labels <- c(
#   "trajectory_type" = "Trajectory structure",
#   "qc_score" = "Overall QC score",
#   "CanRoot" = "Rootable",
#   "CanUse" = "Useful",
#   "Required" = "Required"
# )
#
# labels <- c(application_labels, category_labels, global_labels)
#
# label <- function(x, labels) {
#   labels[x %in% names(labels)] <- labels[x]
#   labels
# }


## Not evaluated reasons --------------
non_inclusion_reasons_footnotes <- tribble(
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

