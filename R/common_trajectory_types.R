trajectory_type_directed <- tribble(
  ~name,
  "directed_linear",
  "directed_cycle",
  "bifurcation",
  "multifurcation",
  "rooted_tree",
  "directed_acyclic_graph",
  "directed_graph"
)

trajectory_type_undirected_to_directed<- c(
  "undirected_linear" = "directed_linear",
  "simple_fork" = "bifurcation",
  "complex_fork" = "multifurcation",
  "unrooted_binary_tree" = "rooted_binary_tree",
  "unrooted_tree" = "rooted_tree",
  "undirected_cycle" = "directed_cycle",
  "undirected_graph" = "directed_acyclic_graph",
  "disconnected_undirected_graph" = "disconnected_directed_graph",
  "unknown" = "unknown"
)
# undirected
trajectory_type_edges_undirected <- tribble(
  ~to, ~from, ~prop_changes,
  "undirected_graph", "undirected_cycle", c("num_branch_nodes == 0", "num_cycles == 1"),
  "simple_fork", "undirected_linear", "num_branch_nodes == 0",
  "complex_fork", "simple_fork", "max_degree == 3",
  "unrooted_binary_tree", "simple_fork", "num_branch_nodes == 1",
  "unrooted_tree", "complex_fork", "num_branch_nodes == 1",
  "unrooted_tree", "unrooted_binary_tree", "max_degree == 3",
  "undirected_graph", "unrooted_tree", "num_cycles == 0",
  "disconnected_undirected_graph", "undirected_graph", "num_components == 1"
) %>% mutate(prop_changes = as.list(prop_changes))
# directed_to_undirected
trajectory_type_edges_undirected_to_directed <- tibble(
  from=names(trajectory_type_undirected_to_directed),
  to = trajectory_type_undirected_to_directed,
  prop_changes = "undirected"
) %>% mutate(prop_changes = list(prop_changes))
# directed (copy over from undirected)
trajectory_type_edges_directed <- tibble(
  from = trajectory_type_undirected_to_directed[trajectory_type_edges_undirected$from],
  to=trajectory_type_undirected_to_directed[trajectory_type_edges_undirected$to],
  prop_changes=trajectory_type_edges_undirected$prop_changes
)


##  ............................................................................
##  Create trajectory types                                                 ####
#' Trajectory types
#' @export
trajectory_types <- {
  trajectory_types <- bind_rows(
    tibble(
      id = names(trajectory_type_undirected_to_directed),
      directedness = "undirected"
    ),
    tibble(
      id = trajectory_type_undirected_to_directed,
      directedness = "directed"
    )
  ) %>% group_by(id) %>% filter(row_number() == 1) %>% ungroup()


  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Colors                                                                  ####

  trajectory_type_colors <- c(
    "undirected_linear" = "#af0dc7",
    "simple_fork" = "#0073d7",
    "unrooted_tree" = "#5ecd2e",
    "unrooted_binary_tree" = "#01FF70",
    "complex_fork" = "#cfbf00",
    "undirected_cycle" = "#39cccc",
    "undirected_graph" = "#ff8821",
    "disconnected_undirected_graph" = "#ff4237",
    "unknown" = "#AAAAAA"
  )
  trajectory_type_colors[trajectory_type_undirected_to_directed[names(trajectory_type_colors)]] <- trajectory_type_colors

  lighten <- function(color, factor=1.4){
    purrr::map_chr(color, function(color) {
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

  trajectory_types$color <- trajectory_type_colors[trajectory_types$id]
  trajectory_types$background_color <- trajectory_type_background_colors[trajectory_types$id]

  trajectory_types
}




##  ............................................................................
##  Create adjacency network of trajectory types                            ####
#' Trajectory type DAG
#' @export
trajectory_type_dag <- {
  trajectory_type_edges <- bind_rows(trajectory_type_edges_undirected, trajectory_type_edges_undirected_to_directed, trajectory_type_edges_directed)

  trajectory_type_dag <- trajectory_type_edges %>% igraph::graph_from_data_frame(vertices=trajectory_types) %>% tidygraph::as_tbl_graph()

  trajectory_type_dag %>% ggraph::ggraph() +
    ggraph::geom_edge_link() +
    ggraph::geom_edge_link(ggplot2::aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2), arrow=ggplot2::arrow()) +
    ggraph::geom_node_label(ggplot2::aes(label=name, fill=directedness)) +
    ggraph::theme_graph()

  trajectory_type_dag
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Ancestors                                                               ####
trajectory_type_ancestors <- trajectory_type_dag %>% igraph::ego(99999999, mode="out") %>% map(names) %>% setNames(names(igraph::V(trajectory_type_dag)))

trajectory_types$ancestors <- trajectory_type_ancestors[trajectory_types$id]

##  ............................................................................
##  Orders                                                                  ####
# undirected_trajectory_type_order <- trajectory_types %>% filter(directedness == "undirected") %>% pull(id) %>% keep(~.!="unknown")
# directed_trajectory_type_order <- trajectory_types %>% filter(directedness == "directed") %>% pull(id) %>% keep(~.!="unknown")
