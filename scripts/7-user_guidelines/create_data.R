library(cowplot)
library(tidyverse)
library(dynbenchmark)

experiment("7-user_guidelines")

read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

methods <-
  left_join(
    methods,
    read_rds(derived_file("implementation_qc_application_scores.rds", "4-method_characterisation")) %>%
      spread("application", "score"),
    "implementation_id"
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
