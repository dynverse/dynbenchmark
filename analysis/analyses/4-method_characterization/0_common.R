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
trajectory_type_background_colors <- lighten(trajectory_type_colors, 3)
