library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("8-compare_topology")

methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
method_trajtypes <- methods %>%
  gather(trajectory_type, can_handle_trajectory_type, !!trajectory_types$id[trajectory_types$directedness == "directed"])

outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))

method_order <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(task_source=="mean") %>%
  arrange(-harm_mean) %>%
  filter(method_id %in% methods$method_id) %>%
  pull(method_id)
ind_scores <- outputs_list$outputs_ind %>%
  mutate(perfect_edge_flip = edge_flip == 1) %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% method_order)

w <- 0.9
topology_sensitivity <- ind_scores %>%
  group_by(method_id, trajectory_type) %>%
  summarise(perc_perfect = mean(perfect_edge_flip)) %>%
  ungroup() %>%
  left_join(trajectory_types, c("trajectory_type"="id")) %>%
  left_join(method_trajtypes, c("trajectory_type", "method_id")) %>%
  mutate(
    method_id=factor(method_id, levels=method_order),
    trajectory_type = factor(trajectory_type, levels=trajectory_types$id)
  ) %>%
  ggplot(aes(ymin=-as.numeric(method_id)-w/2, ymax=-as.numeric(method_id) + w/2, xmax=perc_perfect)) +
  geom_rect(aes(xmax=1, xmin=0, fill=background_color)) +
  geom_rect(aes(fill=color, xmin=0)) +
  geom_rect(aes(xmax=1, xmin=0, color=ifelse(can_handle_trajectory_type, "black", "white")), fill=NA) +
  geom_vline(aes(xintercept = 0.5), linetype="dashed", color="white") +
  facet_grid(.~trajectory_type, labeller=label_facet(label_simple_trajectory_types)) +
  scale_fill_identity() +
  scale_color_identity() +
  # scale_fill_manual(values=set_names(trajectory_types$color, trajectory_types$id)) +
  scale_y_continuous(breaks=-seq_along(method_order), labels=set_names(methods$method_name, methods$method_id)[method_order], expand=c(0.005,0.005)) +
  scale_x_continuous("% of cases where topology was predicted correctly", breaks=c(0.5,1),labels = scales::percent)
topology_sensitivity
topology_sensitivity %>% ggsave(figure_file("topology_sensitivity.svg"), ., width=12, height=12)


# table for correct topology prediction
ind_scores %>%
  group_by(method_id, trajectory_type) %>%
  summarise(perc_perfect = mean(perfect_edge_flip)) %>%
  filter(method_id %in% method_order[1:5]) %>%
  View
therefore
