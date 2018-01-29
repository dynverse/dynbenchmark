## Trajectory types -----------------------------------------
# create the DAG of trajectory types
library(tidygraph)
library(ggraph)
library(tidyverse)
trajectory_type_undirected_to_directed<- c(
  "undirected_linear"="directed_linear",
  "simple_fork"="bifurcation",
  "complex_fork"="multifurcation",
  "unrooted_tree"="rooted_tree",
  "undirected_cycle"="directed_cycle",
  "undirected_graph"="directed_acyclic_graph",
  "disconnected_undirected_graph"="disconnected_directed_graph",
  "unknown"="unknown"
)
trajectory_type_edges_undirected <- tribble(
  ~to, ~from,
  "undirected_graph", "undirected_cycle",
  "simple_fork", "undirected_linear",
  "complex_fork", "simple_fork",
  "unrooted_tree", "complex_fork",
  "undirected_graph", "unrooted_tree",
  "disconnected_undirected_graph", "undirected_graph"
)
trajectory_type_edges_undirected_to_directed <- tibble(from=names(trajectory_type_undirected_to_directed), to = trajectory_type_undirected_to_directed)
trajectory_type_edges_directed <- tibble(
  from = trajectory_type_undirected_to_directed[trajectory_type_edges_undirected$from], to=trajectory_type_undirected_to_directed[trajectory_type_edges_undirected$to]
)

trajectory_type_edges <- bind_rows(trajectory_type_edges_undirected, trajectory_type_edges_undirected_to_directed, trajectory_type_edges_directed)

trajectory_type_nodes <- bind_rows(
  tibble(
    id = names(trajectory_type_undirected_to_directed),
    directedness = "undirected"
  ),
  tibble(
    id = trajectory_type_undirected_to_directed,
    directedness = "directed"
  )
) %>% group_by(id) %>% filter(row_number() == 1)

trajectory_type_dag <- trajectory_type_edges %>% igraph::graph_from_data_frame(vertices=trajectory_type_nodes) %>% as_tbl_graph()

trajectory_type_dag %>% ggraph() +
  geom_edge_link() +
  geom_edge_link(aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2), arrow=arrow()) +
  geom_node_label(aes(label=name, fill=directedness)) +
  theme_graph()

# now find which trajectory types can also handle "lower" trajectory types
trajectory_type_ancestors <- trajectory_type_dag %>% igraph::ego(99999999, mode="out") %>% map(names) %>% setNames(names(igraph::V(trajectory_type_dag)))
trajectory_types <- names(igraph::V(trajectory_type_dag))

# colors
trajectory_type_colors <- c(
  "undirected_linear" = "#af0dc7",
  "simple_fork" = "#0073d7",
  "unrooted_tree" = "#5ecd2e",
  "complex_fork" = "#cfbf00",
  "undirected_cycle" = "#39cccc",
  "undirected_graph" = "#ff8821",
  "disconnected_undirected_graph" = "#ff4237",
  "unknown" = "#AAAAAA"
)
trajectory_type_colors[trajectory_type_undirected_to_directed[names(trajectory_type_colors)]] <- trajectory_type_colors

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
trajectory_type_background_colors <- lighten(trajectory_type_colors, 0.3)


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

