library(dynalysis)
library(ggraph)
library(tidyverse)

experiment("dataset_characterisation/trajectory_types")

trajectory_type_tree <- trajectory_type_dag %>%
  activate(nodes) %>%
  filter(name != "unknown") %>%
  ggraph(layout = "tree") +
  geom_edge_link() +
  geom_edge_link(aes(xend = x+(xend-x)/2, yend = y+(yend - y)/2, filter = ), arrow=arrow(type="closed")) +
  geom_node_label(aes(label=label_long(name), fill=directedness)) +
  theme_graph()


trajectory_type_tree %>% write_rds(figure_file("trajectory_type_tree.rds"))
