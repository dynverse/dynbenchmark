library(cowplot)
library(tidyverse)
library(dynbenchmark)

experiment("09-user_guidelines")

read_rds(derived_file("evaluation_algorithm.rds", "06-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

methods <-
  left_join(
    methods,
    read_rds(derived_file("tool_qc_application_scores.rds", "04-method_characterisation")) %>%
      spread("application", "score"),
    "tool_id"
  ) %>%
  filter(type %in% c("algorithm", "control"))



scoring <- list(
  methods = methods,
  trajtype_scores = trajtype_scores,
  metrics = metrics %>% filter(metric_id %in% c("correlation", "rf_mse", "edge_flip"))
)

process_prior_information <- function(scoring, answer = NULL) {
  scoring
}

add_surveyjs_data <- function(l) {
  l$name = l$question_id
  if(!"title" %in% names(l)) {
    l$title = label_long(l$question_id)
  }
  l
}


saveRDS("")
