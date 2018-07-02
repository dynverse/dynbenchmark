library(dynverse)
library(tidygraph)
library(ggraph)
library(tidyverse)

experiment("2-dataset_characterisation/3-trajectory_type_dag")

trajectory_type_ancestors <- trajectory_type_dag %>% igraph::ego(99999999, mode = "out") %>% map(names) %>% setNames(names(igraph::V(trajectory_type_dag)))

# filter unknown from dag, and add linetype to edges
trajectory_type_tree_data <- trajectory_type_dag %>%
  activate(nodes) %>%
  filter(name != "unknown") %>%
  mutate(num_ancestors = map_dbl(name, function(y) length(trajectory_type_ancestors[[y]]))) %>%
  arrange(num_ancestors, name) %>%
  activate(edges) %>%
  mutate(
    directed_change = ifelse(map_lgl(prop_changes, ~"undirected" %in% .), "Add undirected", "Other generalisation")
  )

# helper function for labelling prop changes
label_prop_changes <- function(prop_changes) {
  format_prop_changes <- function(prop_changes) {
    prop_changes %>%
      str_replace_all("==", " = ") %>%
      str_replace_all("num", "number_of") %>%
      label_long()
  }
  map(prop_changes, format_prop_changes) %>% map_chr(glue::collapse, "\n")
}

less_complex_annotation <-
  ggplot() +
  geom_line(aes(x = 0, y = 0:1),arrow = arrow()) +
  geom_text(aes(x = -0.05, y = 0.5), label = "Increasing complexity",angle = 90) +
  theme_void() +
  scale_x_continuous(limits = c(-0.1, 0.05))

trajectory_type_tree_changes_individual <- map(
  c("undirected", "directed"),
  function(directedness)  {
    gr <- trajectory_type_tree_data %>%
      activate(nodes) %>%
      filter(directedness == !!directedness) %>%
      activate(edges) %>%
      filter(.N()$name[from] != "convergence" | .N()$name[to] != "bifurcation")

    ggraph(gr, layout = "kk") +
      geom_edge_link() +
      geom_edge_link(aes(xend = x+(xend-x)*.25, yend = y+(yend - y)*.25), arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
      geom_node_label(aes(label = label_short(name, 15), fill = colour), color = "white") +
      # ggrepel::geom_label_repel(aes(x = x+(xend-x)/2, y = y+(yend - y)/2, label = label_prop_changes(prop_changes)), data = get_edges()) +
      scale_fill_identity() +
      theme_graph() +
      theme(plot.title = element_text(family = "Open Sans", hjust = 0.5)) +
      scale_x_continuous(expand = c(0.2, 0.2)) +
      ggtitle(label_long(directedness))
  })

trajectory_type_tree_changes <- cowplot::plot_grid(plotlist = c(list(less_complex_annotation), trajectory_type_tree_changes_individual), nrow = 1, rel_widths = c(0.2, 0.4, 0.4))

trajectory_type_tree_changes

trajectory_type_tree_changes %>% write_rds(figure_file("trajectory_type_tree_changes.rds"))


##  ............................................................................
##  Overall tree                                                            ####
trajectory_type_tree_overall <- trajectory_type_tree_data %>%
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_linetype = directed_change)) +
  geom_edge_link(aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2, edge_linetype = directed_change), arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
  geom_node_label(aes(label = label_long(name) %>% gsub(" ", "\n", .), fill = name), color = "white") +
  theme_graph() +
  theme(plot.title = element_text(family = "Open Sans", hjust = 0.5), legend.position = "top") +
  scale_x_continuous(expand = c(0.2, 0.2)) +
  scale_fill_manual(values = set_names(trajectory_types$colour, trajectory_types$id), guide = FALSE) +
  scale_edge_linetype_discrete("", labels = label_long)
trajectory_type_tree_overall

trajectory_type_tree_overall %>% write_rds(figure_file("trajectory_type_tree_overall.rds"))

##  ............................................................................
##  Combined tree plot                                                      ####
trajectory_type_trees <- cowplot::plot_grid(trajectory_type_tree_changes, trajectory_type_tree_overall, ncol = 1, labels = "auto")
trajectory_type_trees
cowplot::save_plot(figure_file("trajectory_type_trees.svg"), trajectory_type_trees, base_width = 15, base_height = 18)
