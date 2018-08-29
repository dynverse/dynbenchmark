library(cowplot)
library(tidyverse)
library(dynbenchmark)

experiment("6-performance_predictors")

read_rds(derived_file("evaluation_algorithm.rds", "06-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

methods <- read_rds(derived_file("methods.rds", experiment_id = "03-methods"))

method_priors <- methods %>%
  filter(method_short_name %in% overall_scores$method_short_name) %>%
  gather(prior_id, prior_usage, !!priors$prior_id) %>%
  select(method_short_name, prior_id, prior_usage) %>%
  filter(prior_usage %in% c("required", "required_default")) %>%
  drop_na(method_short_name) %>%
  {
    bind_rows(
      .,
      tibble(method_short_name = unique(.$method_short_name), prior_id = "any", prior_usage = "required")
    )
  } %>%
  mutate(
    method_short_name = factor(method_short_name, method_order),
    prior_id = factor(prior_id, unique(prior_id))
  )
method_priors <- method_priors %>% complete(method_short_name, prior_id, fill = list(prior_usage = "not_required"))


##  ............................................................................
##  Overal prior comparison                                                 ####
method_priors_scores <- method_priors %>%
  left_join(overall_scores, by = "method_short_name") %>%
  mutate(method_short_name = factor(method_short_name, method_order)) %>%
  gather(score_id, score_value, overall_benchmark)

required_priors_performance_comparison <- method_priors_scores %>%
  ggplot(aes(prior_usage != "not_required", score_value)) +
    geom_violin(aes(fill = prior_usage != "not_required")) +
    geom_point() +
    # geom_label(aes(label = method_short_name)) +
    ggrepel::geom_label_repel(
      aes(label = set_names(methods$method_name, methods$method_short_name)[as.character(method_short_name)]),
      nudge_y = 0.1,
      min.segment.length = 0,
      data = method_priors_scores %>% filter(prior_usage != "not_required") %>% group_by(prior_id, score_id) %>% top_n(1, score_value)
    ) +
    facet_grid(score_id~prior_id, labeller = label_facet()) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf) +
    scale_y_continuous(label_long("overall_benchmark_performance")) +
    scale_x_discrete(label_long("Requires priors"), label = label_long) +
    scale_fill_discrete() +
    theme(panel.spacing = unit(0, "cm"), legend.position = "none")
required_priors_performance_comparison

ggsave(result_file("required_priors_performance_comparison.svg"), required_priors_performance_comparison, width = 14, height = 6)



##  ............................................................................
##  Individual datasets                                                     ####
required_priors_dataset_testing <- indrep_scores %>%
  left_join(method_priors, "method_short_name") %>%
  group_by(dataset_id, prior_id) %>%
  summarise(
    test =
      list(wilcox.test(
        harm_mean[prior_usage == "not_required"],
        harm_mean[prior_usage != "not_required"],
        alternative = "greater",
        conf.int = T
      ))
  ) %>%
  ungroup() %>% # first ungroup before calculating q-values ;)
  mutate(
    p_value = map_dbl(test, "p.value"),
    q_value = p.adjust(p_value, "BH"),
    estimate = map_dbl(test, "estimate"),
    effect = ifelse(estimate > 0,  "↗", "↘")
  )

required_priors_dataset_testing$q_value %>% hist()

datasets_oi <- required_priors_dataset_testing %>%
  arrange(q_value) %>%
  top_n(5, -q_value) %>%
  pull(dataset_id)


indrep_scores %>%
  filter(dataset_id %in% datasets_oi) %>%
  left_join(method_priors) %>%
  ggplot(aes(prior_usage == "not_required", harm_mean)) +
    geom_point() +
    geom_violin() +
    facet_wrap(~dataset_id)
