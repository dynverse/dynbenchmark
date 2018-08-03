## Assess whether the metrics follow certain rules

library(tidyverse)
library(dynbenchmark)

experiment("03-metric_characterisation/01-reference_vs_perturbation")

dataset_design <- read_rds(derived_file("dataset_design.rds"))

# load rules
source(scripts_file("helper-02-rules.R"))

scores <- read_rds(derived_file("scores.rds"))

filter_scores <- function(crossing, scores) {
  scores %>% inner_join(crossing, colnames(crossing))
}

assess_rule <- function(rule, scores) {
  scores <- filter_scores(rule$crossing, scores)

  # extract only the relevant parts
  if (nrow(scores) < nrow(rule$crossing)) {
    warning("Missing some results for ", rule$id, call. = FALSE)
  }

  rule$assessment(scores, rule)
}


assessments <- mapdf(rules, assess_rule, scores=scores) %>% list_as_tibble() %>% mutate(rule_id = rules$id)
assessments$plot_datasets

assessments %>%
  unnest(assessment) %>%
  ggplot(aes(rule_id, metric_id)) +
  geom_tile(aes(fill = check)) +
  scale_fill_manual(values = c(`TRUE` = "#2ECC40", `FALSE` = "#FF4136"))



assessments$plot_scores


## test/create one new rule
rule <- rules %>% extract_row_to_list(which(id == "merge_bifurcation"))
scores <- filter_scores(rule$crossing, scores)
