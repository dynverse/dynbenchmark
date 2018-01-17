trajectory_types <- c("undirected_linear","simple_fork","complex_fork","unrooted_tree","undirected_cycle","undirected_graph")
trajectory_type_colors <- c(
  "undirected_linear" = "#0073d7",
  "simple_fork" = "#5ecd2e",
  "unrooted_tree" = "#ffe900",
  "complex_fork" = "#ff8821",
  "undirected_cycle" = "#c90da9",
  "undirected_graph" = "#ff4237"
)

lighten <- function(color, factor=1.4){
  map_chr(color, function(color) {
    col <- col2rgb(color)
    col <- do.call(rgb2hsv, as.list(col))
    col[1] <- col[1] * 360
    col[2] <- 0.3
    col[3] <- 0.9
    colorspace::hex(do.call(colorspace::HSV, as.list(col)))
  })
}
trajectory_type_background_colors <- set_names(rep("white", length(trajectory_types)), trajectory_types)



categories <- c("availability", "code_quality", "code_assurance", "documentation", "behaviour", "paper")
category_colors <- c("#3498DB", "#E74C3C", "#A4CC2E", "#FEB308", "#B10DC9", "#85144b", "#EA8F10", "#2ECC49", "#CC2E63") %>% .[1:length(categories)] %>% setNames(categories)
category_labels <- setNames(categories %>% gsub("_", " ", .) %>% Hmisc::capitalize(), categories)

global_labels <- c(
  "trajectory_type" = "Trrajectory type",
  "qc_score" = "QC score",
  "CanRoot" = "Rootable",
  "CanUse" = "Useful",
  "Required" = "Required"
)

labels <- c(category_labels, global_labels)


label <- function(x, labels) {
  labels[x %in% names(labels)] <- labels[x]
  labels
}
