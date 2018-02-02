library(dynalysis)
library(tidygraph)
library(ggraph)
library(tidyverse)

experiment("dataset_characterisation/trajectory_types")

trajectory_type_tree <- trajectory_type_dag %>%
  activate(nodes) %>%
  filter(name != "unknown") %>%
  mutate_at(vars(directedness), label_long) %>%
  activate(edges) %>%
  mutate_at(vars(type), label_long) %>%
  ggraph(layout = "tree") +
  geom_edge_link(aes(edge_linetype = type)) +
  geom_edge_link(aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2, edge_linetype = type), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
  geom_node_label(aes(label=label_long(name), fill=directedness)) +
  scale_edge_linetype_manual(values=c(Directedness = "dotted", Generalisation = "solid")) +
  theme_graph() +
  theme(legend.position="top")
trajectory_type_tree

trajectory_type_tree %>% write_rds(figure_file("trajectory_type_tree.rds"))
