library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)
library(igraph)

experiment("6-developer_guidelines")

methods <- read_rds(derived_file("methods.rds", "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))

scores <- outputs_list$outputs_ind %>% filter(method_short_name %in% c("slngsht", "tscan", "scorpius", "recat")) %>% rename(method_id = method_short_name)

trajtype_scores <- outputs_list$outputs_summtrajtype_totals %>% filter(task_source == "mean") %>% rename(method_id = method_short_name)

colors <- trajectory_types %>%
  gather("color_type", "color", color, background_color) %>%
  rename(trajectory_type = id)

trajtype_perfect_edge_flips <- scores %>%
  mutate(perfect_edge_flip=edge_flip==1) %>%
  group_by(method_id, trajectory_type, perfect_edge_flip, task_source) %>%
  count() %>%
  ungroup() %>%
  group_by(trajectory_type, method_id, task_source) %>%
  mutate(perc=n/sum(n)) %>%
  mutate(color_type = ifelse(perfect_edge_flip, "color", "background_color")) %>%
  ungroup() %>%
  left_join(colors, by=c("trajectory_type", "color_type")) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=rev(trajectory_types$id)))

trajtype_perfect_edge_flips_bars <- trajtype_perfect_edge_flips %>%
  ggplot(aes(trajectory_type, perc, fill=color, group=perfect_edge_flip)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_identity() +
  scale_x_discrete(label_long("trajectory_type"), labels=label_long, expand=c(0, 0)) +
  scale_y_continuous(label_long("Percentage"), expand=c(0, 0)) +
  coord_flip() +
  facet_grid(method_id~task_source)
trajtype_perfect_edge_flips_bars

## Still have to add the opposite here, ~ false positive rate

trajtype_perfect_edge_flips_bars %>% write_rds(figure_file("trajtype_perfect_edge_flips.rds"))
