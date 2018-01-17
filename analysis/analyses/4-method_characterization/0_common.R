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

global_labels <- c(
  "trajectory_type" = "Trajectory structure",
  "qc_score" = "Overall QC score",
  "CanRoot" = "Rootable",
  "CanUse" = "Useful",
  "Required" = "Required"
)

labels <- c(application_labels, category_labels, global_labels)

label <- function(x, labels) {
  labels[x %in% names(labels)] <- labels[x]
  labels
}
