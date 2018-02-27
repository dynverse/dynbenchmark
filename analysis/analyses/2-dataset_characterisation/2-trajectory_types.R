library(dynalysis)
library(tidygraph)
library(ggraph)
library(tidyverse)

experiment("2-dataset_characterisation/2-trajectory_types")



#   ____________________________________________________________________________
#   Trajectory type trees                                                   ####

trajectory_type_tree_data <- trajectory_type_dag %>%
  activate(nodes) %>%
  filter(name != "unknown") %>%
  activate(edges) %>%
  mutate(
    directed_change = map_lgl(prop_changes, ~"undirected" %in% .),
    directed_change = ifelse(directed_change,"Add undirected", "Other generalisation")
  )


##  ............................................................................
##  Changes                                                                 ####
label_prop_changes <- function(prop_changes) {
  format_prop_changes <- function(prop_changes) {
    gsub("==", "=", prop_changes) %>% gsub("num", "number_of", .) %>% label_long()
  }
  map(prop_changes, format_prop_changes) %>% map_chr(glue::collapse, "\n")
}

trajectory_type_tree_changes <- trajectory_type_tree_data %>%
  activate(nodes) %>%
  filter(directedness == "undirected") %>%
  ggraph(layout = "tree") +
  geom_edge_link() +
  geom_edge_link(aes(xend = x+(xend-x)/1.3, yend = y+(yend - y)/1.3), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
  geom_node_label(aes(label=label_long(name), fill=name)) +
  # ggrepel::geom_label_repel(aes(x=x+(xend-x)/2, y = y+(yend - y)/2, label=label_prop_changes(prop_changes)), data = get_edges(), min.segment.length=Inf, force=0.1) +
  scale_fill_manual(values=set_names(trajectory_types$color, trajectory_types$id)) +
  theme_graph() +
  theme(legend.position="none") +
  scale_x_continuous(expand=c(0.2, 0.2))
trajectory_type_tree_changes

trajectory_type_tree_changes %>% write_rds(figure_file("trajectory_type_tree_changes.rds"))


##  ............................................................................
##  Overall tree                                                            ####
trajectory_type_tree_overall <- trajectory_type_tree_data %>%
  ggraph(layout = "tree") +
  geom_edge_link(aes(edge_linetype = directed_change)) +
  geom_edge_link(aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2, edge_linetype = directed_change), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
  geom_node_label(aes(label=label_long(name) %>% gsub(" ", "\n", .), fill=directedness)) +
  theme_graph() +
  theme(legend.position="top") +
  scale_x_continuous(expand=c(0.2, 0.2)) +
  scale_fill_discrete("", labels=label_long) +
  scale_edge_linetype_discrete("", labels=label_long)
trajectory_type_tree_overall

trajectory_type_tree_overall %>% write_rds(figure_file("trajectory_type_tree_overall.rds"))

##  ............................................................................
##  Combined tree plot                                                      ####
less_complex_annotation <- ggplot() +
  geom_line(aes(x=0, y=0:1),arrow=arrow()) +
  geom_text(aes(x=-0.05, y=0.5), label="Increasing complexity",angle=90) +
  theme_void() +
  scale_x_continuous(limits=c(-0.1, 0.05))


trajectory_type_trees <- cowplot::plot_grid(
  less_complex_annotation,
  trajectory_type_tree_changes,
  trajectory_type_tree_overall,
  ncol=3,
  labels=c("", "a","b"),
  rel_widths = c(0.1, 0.4, 0.4)
)
trajectory_type_trees
save_plot(figure_file("trajectory_type_trees.svg"), trajectory_type_trees, base_width=15, base_height=10)
