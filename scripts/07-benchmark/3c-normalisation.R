## CHECK VARIANCES PER DATASET AND METRIC
stat_funs <- c("var", "mean")
metricso <- c("overall", metrics)

dat_df <-
  data %>%
  select(method_id, dataset_id, !!metricso) %>%
  gather(metric, score, !!metricso) %>%
  group_by(dataset_id, metric) %>%
  filter(n() > 2) %>%
  rename(unnorm = score) %>%
  mutate(norm = dynbenchmark:::.benchmark_aggregate_normalisation$scalesigmoid(unnorm)) %>%
  gather(type, score, unnorm, norm) %>%
  mutate(type = factor(type, levels = c("unnorm", "norm"))) %>%
  ungroup()

var_df <-
  dat_df %>%
  group_by(type, dataset_id, metric) %>%
  summarise_at(vars(score), stat_funs) %>%
  ungroup()

g <- ggplot(var_df) +
  geom_point(aes(mean, var, colour = metric)) +
  facet_wrap(~type) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()

# g
ggsave(result_file("normalisation_var_mean.pdf"), g, width = 10, height = 5)

rm(stat_funs, metricso, dat_df, var_df, g)
