## Assess whether the metrics follow certain rules

library(tidyverse)
library(dynbenchmark)

experiment("03-metric_characterisation/01-metric_conformity")

dataset_design <- read_rds(derived_file("dataset_design.rds"))

# load scores and models
scores <- read_rds(derived_file("scores.rds"))
models <- read_rds(derived_file("models.rds"))

metrics <- unique(scores$metric_id)

# load rules
source(scripts_file("helper-rules.R"))

calculate_harmonic_mean <- function(...) {
  inputs <- list(...)

  testthat::expect_equal(length(unique(map_int(inputs, length))), 1)

  n <- length(inputs)
  x <- do.call(cbind, inputs)

  n / rowSums(1/x)
}

scores <- scores %>% bind_rows(
  scores %>%
    spread(metric_id, score) %>%
    mutate(harm_mean = calculate_harmonic_mean(correlation, edge_flip, lm_nmse, featureimp_cor)) %>%
    gather("metric_id", "score", harm_mean) %>%
    select(-one_of(metrics))
)

# functions to assess conformity
filter_based_on_crossing <- function(x, crossing) {
  if (!all(colnames(crossing) %in% colnames(x))) {
    stop("All colnames in crossing should also be present in x")
  }

  y <- x %>% inner_join(crossing, colnames(crossing))

  if (nrow(y) < nrow(crossing)) {
    warning("Missing some results for: ", crossing %>% anti_join(x, colnames(crossing)), call. = FALSE)
  }

  y
}

assess_conformity <- function(rule, scores, models) {
  # extract only the relevant parts
  scores <- filter_based_on_crossing(scores, rule$crossing)
  models <- filter_based_on_crossing(models, rule$crossing)

  if (nrow(scores) == 0) {
    warning(rule$id)
  } else {
    rule$assessment(scores, rule, models)
  }
}


assessments <- mapdf(rules, assess_conformity, scores=scores, models=models) %>% list_as_tibble() %>% mutate(rule_id = rules$id)



assessments %>%
  unnest(conformity) %>%
  ggplot(aes(metric_id, rule_id)) +
  geom_tile(aes(fill = conforms)) +
  scale_fill_manual(values = c(`TRUE` = "#2ECC40", `FALSE` = "#FF4136", `NA` = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mapdf(assessments, function(assessment) {
  rmarkdown::render(
    scripts_file("04-simple_conformity_report.Rmd"),
    params = lst(assessment),
    output_file = glue::glue("{assessment$rule_id}.html"),
    output_dir = result_file("reports")
  )
})




rule <- rules %>% filter(id == "equal_identity") %>% extract_row_to_list(1)
assess_conformity(rule, scores, models)$plot_scores
assess_conformity(rule, scores, models)$plot_datasets




rule <- rules %>% filter(id == "filter_cells") %>% extract_row_to_list(1)
assess_conformity(rule, scores, models)$plot_scores


#
#
#
#
# ## test/create one new rule
# rule <- rules %>% extract_row_to_list(which(id == "merge_bifurcation"))
# assess_conformity(rule, scores, models)
