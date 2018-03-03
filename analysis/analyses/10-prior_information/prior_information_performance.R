library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("10-prior_information")

scores <- c("harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse")

methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
method_scores <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id) %>%
  filter(task_source=="mean")
indrep_scores <- outputs_list$outputs_summrepl %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id)
method_order <- method_scores %>%
  arrange(-harm_mean) %>%
  filter(method_id %in% methods$method_id) %>%
  pull(method_id)

method_priors <- methods %>%
  filter(method_id %in% method_scores$method_id) %>%
  gather(prior_id, prior_usage, !!priors$prior_id) %>%
  select(method_id, prior_id, prior_usage) %>%
  filter(prior_usage %in% c("required", "required_default")) %>%
  drop_na(method_id) %>%
  {
    bind_rows(
      .,
      tibble(method_id = unique(.$method_id), prior_id = "any", prior_usage="required")
    )
  } %>%
  mutate(
    method_id = factor(method_id, method_order),
    prior_id = factor(prior_id, unique(prior_id))
  )
method_priors <- method_priors %>% complete(method_id, prior_id, fill = list(prior_usage="not_required"))


##  ............................................................................
##  Overal prior comparison                                                 ####
method_priors_scores <- method_priors %>%
  left_join(method_scores, by="method_id") %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  gather(score_id, score_value, !!scores)

required_priors_performance_comparison <- method_priors_scores %>%
  ggplot(aes(prior_usage != "not_required", score_value)) +
    geom_violin(aes(fill=prior_usage != "not_required")) +
    geom_point() +
    # geom_label(aes(label = method_id)) +
    ggrepel::geom_label_repel(
      aes(label=set_names(methods$method_name, methods$method_id)[as.character(method_id)]),
      nudge_y = 0.1,
      min.segment.length=0,
      data=method_priors_scores %>% filter(prior_usage != "not_required") %>% group_by(prior_id, score_id) %>% top_n(1, score_value)
    ) +
    facet_grid(score_id~prior_id, labeller=label_facet()) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
    scale_y_continuous(label_long("performance")) +
    scale_x_discrete(label_long("Requires priors"), label=label_long) +
    scale_fill_discrete() +
    theme(panel.spacing=unit(0, "cm"), legend.position="none")
required_priors_performance_comparison

ggsave(figure_file("required_priors_performance_comparison.svg"), required_priors_performance_comparison, width=14, height=6)



##  ............................................................................
##  Individual datasets                                                     ####
required_priors_dataset_testing <- indrep_scores %>%
  left_join(method_priors, "method_id") %>%
  group_by(task_id, prior_id) %>%
  summarise(
    test =
      list(wilcox.test(
        harm_mean[prior_usage == "not_required"],
        harm_mean[prior_usage != "not_required"],
        alternative="greater",
        conf.int=T
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

tasks_oi <- required_priors_dataset_testing %>%
  arrange(q_value) %>%
  top_n(5, -q_value) %>%
  pull(task_id)


indrep_scores %>%
  filter(task_id %in% tasks_oi) %>%
  left_join(method_priors) %>%
  ggplot(aes(prior_usage == "not_required", harm_mean)) +
    geom_point() +
    geom_violin() +
    facet_wrap(~task_id)
