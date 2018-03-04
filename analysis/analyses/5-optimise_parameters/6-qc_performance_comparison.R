library(cowplot)
library(tidyverse)
library(dynalysis)

experiment("5-optimise_parameters/6-qc_performance_comparison")

implementation_qc <- readRDS(derived_file("implementation_qc.rds", "4-method_characterisation"))
methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
method_performance <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id) %>%
  filter(task_source=="mean")

# overall scores
method_scores <- outputs_list$outputs_summmethod_totals %>% filter(task_source == "mean") %>% {setNames(.$harm_mean, .$method_short_name)}
method_scores <- method_scores[intersect(methods$method_id, names(method_scores))]

# overall qc scores
method_qc_overall <- methods %>% select(method_id, qc_score)

# individual item qc scores
implementation_qc_ind <- readRDS(derived_file("implementation_qc.rds", "4-method_characterisation"))
method_qc_ind <- implementation_qc_ind %>%
  left_join(methods %>% select(method_id, implementation_id, qc_score), "implementation_id") %>%
  left_join(method_performance, "method_id") # convert to qc at level of methods

method_scores <- left_join(method_performance, methods, "method_id")
qc_performance_comparison <- method_scores %>%
  ggplot(aes(harm_mean, qc_score)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label=methods$method_name[match(method_id, methods$method_id)]), method_scores %>% filter(rank(qc_score) > n()-5 | rank(harm_mean) > n()-5), min.segment.length = 0) +
    labs(x=label_long("harm_mean"), y=label_long("qc_score"))
qc_performance_comparison
qc_performance_comparison %>% ggsave(figure_file("qc_performance_comparison.svg"), ., width=6, height=6)

# correlate qc items with performance
method_qc_performance_testing <- method_qc_ind %>%
  mutate(perfect = answer == 1) %>%
  group_by(check_id) %>%
  filter(sum(perfect) >= 1 & (n() - sum(perfect)) >= 1) %>%  # require in every group enough observations
  summarise(
    test =
      list(wilcox.test(
        harm_mean[answer == 1],
        harm_mean[answer < 1],
        conf.int=T
      ))
  ) %>%
  ungroup() %>% # first ungroup before calculating q-values ;)
  mutate(
    p_value = map_dbl(test, "p.value"),
    q_value = p.adjust(p_value, "BH"),
    estimate = map_dbl(test, "estimate"),
    effect = ifelse(estimate > 0,  "↗", "↘")
  ) %>%
  left_join(checks)
