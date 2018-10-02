#' An overview of all the topologies present in the datasets

library(dynbenchmark)
library(tidyverse)

experiment("01-datasets/04-dataset_characterisation")

datasets <- load_datasets()

##  ............................................................................
##  Clustering the topologies                                               ####
# - first simplifying the graph
# - then applying hierarchical clustering with distance cutoff 0
cluster_topologies <- function(milestone_networks) {
  # group datasets by in and out degree distribution (after simplification)
  graph_simplified <- map(
    milestone_networks,
    function(milestone_network) {
      milestone_network %>%
        igraph::graph_from_data_frame(directed = any(milestone_network$directed)) %>%
        dynwrap::simplify_igraph_network(allow_duplicated_edges = FALSE, allow_self_loops = FALSE)
    }
  )

  max_degree <- map_dbl(graph_simplified, function(graph) {max(igraph::degree(graph))}) %>% max()
  dataset_degree_distributions <- map(
    graph_simplified,
    function(graph) {
      degree_distribution_in <- igraph::degree.distribution(graph, mode = "in")
      if (length(degree_distribution_in) < max_degree + 1) {
        degree_distribution_in <- c(rep(0, max_degree - length(degree_distribution_in) + 1), degree_distribution_in)
      }

      degree_distribution_out <- igraph::degree.distribution(graph, mode = "out")
      if (length(degree_distribution_out) < max_degree + 1) {
        degree_distribution_out <- c(rep(0, max_degree - length(degree_distribution_out) + 1), degree_distribution_out)
      }
      c(
        degree_distribution_in,
        degree_distribution_out
      )
    }
  ) %>% do.call(rbind, .)

  tibble(
    graph_simplified,
    topology_cluster = hclust(dist(dataset_degree_distributions)) %>%
      cutree(h = 0)
  )
}

datasets_clustered <- bind_cols(datasets, cluster_topologies(datasets$milestone_network))

topology_clusters <- datasets_clustered %>%
  group_by(topology_cluster) %>%
  summarise(
    graph_simplified = graph_simplified[sample(n(), 1)],
    n = n(),
    trajectory_type = first(trajectory_type),
    ids = list(id)
  ) %>%
  ungroup() %>%
  arrange(
    match(trajectory_type, trajectory_types$id),
    map_int(graph_simplified, ~length(igraph::V(.)))
  )


plots_topology_clusters <- topology_clusters %>%
  nest(-trajectory_type, .key = "topology_clusters") %>%
  mutate(plot_topology_clusters = map2(
    trajectory_type,
    topology_clusters,
    function(
      trajectory_type,
      topology_clusters
    ) {
      individual_plots <- pmap(
        topology_clusters,
        function(graph_simplified, n, ...) {
          if (igraph::is_directed(graph_simplified)) {
            arrow <- arrow(type = "closed", length = unit(0.1, "inches"))
          } else {
            arrow = NULL
          }

          ggraph::ggraph(graph_simplified) +
            ggraph::geom_node_point() +
            ggraph::geom_edge_link(arrow = arrow) +
            ggtitle(n) +
            theme_graph() +
            theme(plot.title = element_text(hjust = 0.5))
        }
      )

      ncol <- 8

      patchwork::wrap_plots(
        individual_plots,
        ncol = min(c(length(individual_plots), ncol)),
        nrow = ceiling(length(individual_plots) / ncol)
      )
    }
  )) %>%
  mutate(
    n = map_int(topology_clusters, nrow),
    n_datasets = map_int(topology_clusters, ~sum(.$n))
  )

plots_topology_clusters %>%
  write_rds(result_file("topology_clusters.rds"))
