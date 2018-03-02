trajectory_type_directed_to_undirected<- c(
  "directed_linear" = "undirected_linear",
  "bifurcation" = "simple_fork",
  "convergence" = "simple_fork",
  "multifurcation" = "complex_fork",
  "rooted_binary_tree" = "unrooted_binary_tree",
  "rooted_tree" = "unrooted_tree",
  "directed_cycle" = "undirected_cycle",
  "directed_graph" = "undirected_graph",
  "directed_acyclic_graph" = "undirected_graph",
  "disconnected_directed_graph" = "disconnected_undirected_graph",
  "unknown" = "unknown"
)
# undirected
trajectory_type_edges_undirected <- tribble(
  ~to, ~from, ~prop_changes,
  "simple_fork", "undirected_linear", "num_branch_nodes == 0",
  "complex_fork", "simple_fork", "max_degree == 3",
  "unrooted_binary_tree", "simple_fork", "num_branch_nodes == 1",
  "unrooted_tree", "complex_fork", "num_branch_nodes == 1",
  "unrooted_tree", "unrooted_binary_tree", "max_degree == 3",
  "undirected_graph", "unrooted_tree", "num_cycles == 0",
  "disconnected_undirected_graph", "undirected_graph", "num_components == 1",
  "undirected_graph", "undirected_cycle", c("num_branch_nodes == 0", "num_cycles == 1")
) %>% mutate(prop_changes = as.list(prop_changes))
# directed_to_undirected
trajectory_type_edges_undirected_to_directed <- tibble(
  from = trajectory_type_directed_to_undirected,
  to = names(trajectory_type_directed_to_undirected),
  prop_changes = "undirected"
) %>% mutate(prop_changes = list(prop_changes))
# directed (copy over from undirected)

# TODO: the following prop_changes are incomplete!!
trajectory_type_edges_directed <- tribble(
  ~to, ~from, ~prop_changes,
  "bifurcation", "directed_linear", "",
  "bifurcation", "convergence", "",
  "convergence", "bifurcation", "",
  "convergence", "directed_linear", "",
  "multifurcation", "bifurcation", "",
  "rooted_binary_tree", "bifurcation", "",
  "rooted_tree", "multifurcation", "",
  "rooted_tree", "rooted_binary_tree", "",
  "directed_acyclic_graph", "rooted_tree", "",
  "directed_acyclic_graph", "convergence", "",
  "directed_graph", "directed_acyclic_graph", "",
  "disconnected_directed_graph", "directed_graph", "",
  "directed_graph", "directed_cycle", ""
) %>% mutate(prop_changes = as.list(prop_changes))

##  ............................................................................
##  Create trajectory types                                                 ####
#' Trajectory types
#' @export
trajectory_types <- {
  trajectory_types <- bind_rows(
    tibble(
      id = unique(c(trajectory_type_edges_undirected$from, trajectory_type_edges_undirected$to)),
      directedness = "undirected"
    ),
    tibble(
      id = unique(c(trajectory_type_edges_directed$from, trajectory_type_edges_directed$to)),
      directedness = "directed"
    ),
    tibble(
      id = "unknown",
      directedness = "unknown"
    )
  )


  ##  ............................................................................
  ##  Simplified names                                                        ####
  trajectory_types <- trajectory_types %>%
    mutate(
      simplified =
        gsub("undirected_", "", id) %>%
        gsub("directed_", "", .) %>%
        gsub("unrooted_", "", .) %>%
        gsub("rooted_", "", .) %>%
        gsub("simple_fork", "bifurcation", .) %>%
        gsub("complex_fork", "multifurcation", .)
    )


  ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
  ### Colors                                                                  ####

  trajectory_type_colors <- tribble(
    ~id, ~color,
    "directed_linear", "#0278dd",
    "bifurcation" , "#3ad1d1",
    "convergence", "#1f8888",
    "rooted_tree" , "#e0ab00",
    "rooted_binary_tree" , "#00b009",
    "multifurcation" , "#7fbe00",
    "directed_cycle" , "#003d76",
    "directed_acyclic_graph" , "#ff8821",
    "directed_graph" , "#ff4237",
    "disconnected_directed_graph" , "#ca0565",
    "unknown" , "#AAAAAA"
  )

  trajectory_type_colors <- trajectory_type_colors %>%
    left_join(trajectory_type_edges_undirected_to_directed, by=c("id" = "to")) %>%
    select(-id) %>%
    rename(id = from) %>%
    select(id, color) %>%
    group_by(id) %>%
    summarise(color = first(color)) %>%
    bind_rows(trajectory_type_colors)

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
  trajectory_type_colors$background_color <- lighten(trajectory_type_colors$color, 0.3)

  trajectory_types <- left_join(trajectory_types, trajectory_type_colors, "id") %>%
    group_by(id) %>%
    filter(row_number() == 1) %>%
    ungroup()

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

trajectory_types <- trajectory_types %>% arrange(unlist(map(ancestors, length))) # order according to number of ancestors

##  ............................................................................
##  Orders                                                                  ####
# undirected_trajectory_type_order <- trajectory_types %>% filter(directedness == "undirected") %>% pull(id) %>% keep(~.!="unknown")
# directed_trajectory_type_order <- trajectory_types %>% filter(directedness == "directed") %>% pull(id) %>% keep(~.!="unknown")
