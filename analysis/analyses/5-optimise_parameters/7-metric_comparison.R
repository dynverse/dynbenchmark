library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("5-optimise_parameters/7-metric_comparison")

extra_methods <- tibble(
  method_id = c("manual_robrechtc", "manual_wouters", "identity", "shuffle", "random"),
  maximal_trajectory_type = "unknown",
  method_type = c("manual", "manual", "control", "control", "control")
)
methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation")) %>%
  mutate(method_type = "algorithm") %>%
  bind_rows(extra_methods)


outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))

metrics_sel <- c("rank_correlation" = "Correlation", "rank_edge_flip" = "Edge flip", "rank_rf_mse" = "RF MSE")

# create trajectory type
trajtype_metric_scores <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(task_source == "mean") %>%
  rename(method_id = method_short_name) %>%
  select(method_id, trajectory_type = trajectory_type_f, one_of(names(metrics_sel))) %>%
  gather(metric, score, one_of(names(metrics_sel))) %>%
  mutate(metric_lab = metrics_sel[metric]) %>%
  drop_na()

cross_df <- crossing(metric_left = names(metrics_sel), metric_right = names(metrics_sel)) %>%
  filter(metric_left < metric_right) %>%
  left_join(trajtype_metric_scores %>% rename(metric_left = metric, metric_lab_left = metric_lab, score_left = score), by = c("metric_left")) %>%
  left_join(trajtype_metric_scores %>% rename(metric_right = metric, metric_lab_right = metric_lab, score_right = score), by = c("metric_right", "method_id", "trajectory_type")) %>%
  mutate(
    facet_metric = paste0(metric_lab_left, " vs ", metric_lab_right),
    trajectory_type_l = factor(label_long(trajectory_type), levels = label_long(levels(trajectory_type)))
  )

g <- ggplot(cross_df) +
  geom_point(aes(score_right, score_left, colour = trajectory_type)) +
  facet_grid(facet_metric ~ trajectory_type_l) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  coord_equal() +
  labs(x = "Score B (see right facet label)", y = "Score A (see left facet label)") +
  scale_color_manual(values=set_names(c("black", "black", trajectory_types$color), c("all", "overall", trajectory_types$id))) +
  theme(legend.position = "none")

ggsave(figure_file("metrics_comparison.svg"), g, width = 14, height = 5)


# # create overall
# source_score <- outputs_list$outputs_summmethod_totals %>%
#   rename(method_id = method_short_name) %>%
#   select(method_id, task_source, harm_mean) %>%
#   spread("task_source", "harm_mean") %>%
#   drop_na() %>%
#   mutate(trajectory_type = "overall")
#
# # create all
# trajtype_source_scores <- bind_rows(
#   trajtype_source_scores,
#   trajtype_source_scores %>% mutate(trajectory_type = "all"),
#   source_score
# ) %>%
#   left_join(methods, "method_id") #%>%
#   # filter(method_type == "algorithm")
#
# # set order
# trajtype_source_scores <- trajtype_source_scores %>%
#   mutate(trajectory_type = factor(trajectory_type, levels=c("all", "overall", trajectory_types$id)))
#
# # calculate correlations
# trajtype_source_score_cors <- trajtype_source_scores %>%
#   group_by(trajectory_type) %>%
#   summarise(cor=cor(real, synthetic))
#
# real_synthetic_comparison <- trajtype_source_scores %>%
#   ggplot(aes(real, synthetic)) +
#   geom_point(aes(color=trajectory_type)) +
#   geom_text(aes(label=round(cor, 2)), x=0.1, y=0.9, data=trajtype_source_score_cors, size=5) +
#   facet_wrap(~trajectory_type, labeller = label_facet(), nrow=2) +
#   scale_color_manual(values=set_names(c("black", "black", trajectory_types$color), c("all", "overall", trajectory_types$id))) +
#   scale_x_continuous(breaks=c(0,1))+
#   scale_y_continuous(breaks=c(0,1))+
#   coord_equal() +
#   labs(x="Real", y="Synthetic")+
#   theme(legend.position="none")
# real_synthetic_comparison
# real_synthetic_comparison %>% write_rds(figure_file("real_synthetic_comparison.rds"))
# real_synthetic_comparison %>% ggsave(figure_file("real_synthetic_comparison.svg"), ., width=15, height=5)
