library(cowplot)
library(tidyverse)
library(dynbenchmark)

library(tidygraph)
library(ggraph)

experiment("8-compare_topology")

methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
method_trajtypes <- methods %>%
  gather(trajectory_type, can_handle_trajectory_type, !!trajectory_types$id[trajectory_types$directedness == "directed"])

read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

ind_scores <- ind_scores %>%
  mutate(perfect_edge_flip = edge_flip == 1)

w <- 0.9

topology_sensitivity <- ind_scores %>%
  group_by(method_id, trajectory_type) %>%
  summarise(perc_perfect = mean(perfect_edge_flip)) %>%
  ungroup() %>%
  select(trajectory_type, method_id, perc_perfect)
topology_sensitivity %>% saveRDS(result_file("topology_sensitivity.rds"))



topology_sensitivity_plot <- topology_sensitivity %>%
  left_join(trajectory_types, c("trajectory_type" = "id")) %>%
  left_join(method_trajtypes, c("trajectory_type", "method_id")) %>%
  mutate(
    method_id = factor(method_id, levels = method_order),
    trajectory_type = factor(trajectory_type, levels = trajectory_types$id)
  ) %>%
  ggplot(aes(ymin = -as.numeric(method_id)-w/2, ymax = -as.numeric(method_id) + w/2, xmax = perc_perfect)) +
  geom_rect(aes(xmax = 1, xmin = 0, fill = background_color)) +
  geom_rect(aes(fill = color, xmin = 0)) +
  geom_rect(aes(xmax = 1, xmin = 0, color = ifelse(can_handle_trajectory_type, "black", "white")), fill = NA) +
  geom_vline(aes(xintercept = 0.5), linetype = "dashed", color = "white") +
  facet_grid(.~trajectory_type, labeller = label_facet(label_simple_trajectory_types)) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_bw() +
  theme(panel.spacing.x = unit(0, "inch"), panel.grid.major = element_blank()) +
  # scale_fill_manual(values = set_names(trajectory_types$colour, trajectory_types$id)) +
  scale_y_continuous(breaks = -seq_along(method_order), labels = set_names(methods$method_name, methods$method_id)[method_order], expand = c(0.005,0.005)) +
  scale_x_continuous("% of cases where topology was predicted correctly", breaks = c(0.5,1),labels = scales::percent)
topology_sensitivity_plot
topology_sensitivity_plot %>% ggsave(figure_file("topology_sensitivity.svg"), ., width = 12, height = 12)


# table for correct topology prediction
topology_sensitivity_table <- map(c("html", "latex"), function(format) {
  ind_scores %>%
    mutate(trajectory_type = factor(trajectory_type, trajectory_types$id)) %>%
    mutate(method_id = factor(method_id, method_order)) %>%
    arrange(method_id) %>%
    group_by(method_id, trajectory_type) %>%
    summarise(perc_perfect = mean(perfect_edge_flip)) %>%
    spread(trajectory_type, perc_perfect) %>%
    ungroup() %>%
    mutate_at(
      vars(-method_id),
      ~kableExtra::cell_spec(
        scales::percent(.),
        format,
        background = kableExtra::spec_color(., scale_from = c(0, 1)),
        color = ifelse(. > 0.5, "black", "white")
    )) %>%
    mutate(method_id = methods$method_name[match(method_id, methods$method_id)]) %>%
    rename_at(vars(-method_id), ~trajectory_types$simplified[match(., trajectory_types$id)]) %>%
    rename_all(label_long) %>%
    knitr::kable(format, escape = F) %>%
    kableExtra::kable_styling()
}) %>% set_names(c("html", "latex"))
topology_sensitivity_table
write_rds(topology_sensitivity_table, figure_file("topology_sensitivity_table.rds"))

