library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("5-optimise_parameters/5-real_synthetic_comparison")

extra_methods <- tibble(
  method_id = c("manual_robrechtc", "manual_wouters", "identity", "shuffle", "random"),
  maximal_trajectory_type = "unknown",
  method_type = c("manual", "manual", "control", "control", "control")
)
methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation")) %>%
  mutate(method_type = "algorithm") %>%
  bind_rows(extra_methods)


outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))

# create trajectory type
trajtype_source_scores <- outputs_list$outputs_summtrajtype_totals %>%
  filter(task_source %in% c("real", "synthetic")) %>%
  rename(method_id = method_short_name) %>%
  select(method_id, trajectory_type, task_source, harm_mean) %>%
  spread("task_source", "harm_mean") %>%
  drop_na()

# create overall
source_score <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  select(method_id, task_source, harm_mean) %>%
  spread("task_source", "harm_mean") %>%
  drop_na() %>%
  mutate(trajectory_type = "overall")

# create all
trajtype_source_scores <- bind_rows(
  trajtype_source_scores,
  trajtype_source_scores %>% mutate(trajectory_type = "all"),
  source_score
) %>%
  left_join(methods, "method_id") #%>%
  # filter(method_type == "algorithm")

# set order
trajtype_source_scores <- trajtype_source_scores %>%
  mutate(trajectory_type = factor(trajectory_type, levels=c("all", "overall", trajectory_types$id)))

# calculate correlations
trajtype_source_score_cors <- trajtype_source_scores %>%
  group_by(trajectory_type) %>%
  summarise(cor=cor(real, synthetic))

real_synthetic_comparison <- trajtype_source_scores %>%
  ggplot(aes(real, synthetic)) +
  geom_point(aes(color=trajectory_type)) +
  geom_text(aes(label=round(cor, 2)), x=0.1, y=0.9, data=trajtype_source_score_cors, size=5) +
  facet_wrap(~trajectory_type, labeller = label_facet(), nrow=2) +
  scale_color_manual(values=set_names(c("black", "black", trajectory_types$color), c("all", "overall", trajectory_types$id))) +
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(breaks=c(0,1))+
  coord_equal() +
  labs(x="Real", y="Synthetic")+
  theme(legend.position="none")
real_synthetic_comparison
real_synthetic_comparison %>% write_rds(figure_file("real_synthetic_comparison.rds"))
real_synthetic_comparison %>% ggsave(figure_file("real_synthetic_comparison.svg"), ., width=15, height=5)
