library(cowplot)
library(tidyverse)
library(dynbenchmark)

library(tidygraph)
library(ggraph)

experiment("11-evaluation_robustness")

read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

# create trajectory type
trajtype_source_scores <- trajtype_scores %>%
  filter(dataset_source %in% c("real", "synthetic")) %>%
  select(method_short_name, trajectory_type, dataset_source, harm_mean) %>%
  spread("dataset_source", "harm_mean") %>%
  drop_na()

# create overall
source_scores <- source_scores %>%
  select(method_short_name, dataset_source, harm_mean) %>%
  spread("dataset_source", "harm_mean") %>%
  drop_na() %>%
  mutate(trajectory_type = "overall")

# create all
trajtype_source_scores <- bind_rows(
  trajtype_source_scores,
  trajtype_source_scores %>% mutate(trajectory_type = "all"),
  source_scores
) %>%
  inner_join(methods, "method_short_name") #%>%
# filter(method_type == "algorithm")

# set order
trajtype_source_scores <- trajtype_source_scores %>%
  mutate(trajectory_type = factor(trajectory_type, levels = c("all", "overall", trajectory_types$id)))

# calculate correlations
trajtype_source_score_cors <- trajtype_source_scores %>%
  group_by(trajectory_type) %>%
  summarise(cor = cor(real, synthetic))

real_synthetic_comparison <- trajtype_source_scores %>%
  ggplot(aes(real, synthetic)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(color = trajectory_type)) +
  geom_text(aes(label = round(cor, 2)), x = 0.1, y = 0.9, data = trajtype_source_score_cors) +
  facet_wrap(~trajectory_type, labeller = label_facet(label_simple_trajectory_types), nrow = 1) +
  scale_color_manual(values = set_names(c("black", "black", trajectory_types$colour), c("all", "overall", trajectory_types$id))) +
  scale_x_continuous(breaks = c(0,1), limits = c(0, 1))+
  scale_y_continuous(breaks = c(0,1), limits = c(0, 1))+
  coord_equal() +
  labs(x = label_wrap("Overall performance on real data", 100), y = label_wrap("Overall performance on synthetic data", 25))+
  theme_bw() +
  theme(
    legend.position = "none", panel.grid = element_blank()
  )
real_synthetic_comparison
real_synthetic_comparison %>% write_rds(figure_file("real_synthetic_comparison.rds"))
real_synthetic_comparison %>% ggsave(figure_file("real_synthetic_comparison.svg"), ., width = 15, height = 5)
