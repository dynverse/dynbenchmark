## Assess whether the metrics follow certain rules

# load rules
source(scripts_file("helper-02-rules.R"))

assess_rule <- function(rule, scores) {
  # extract only the relevant parts
  scores <- scores %>% inner_join(rule$crossing, colnames(rule$crossing))

  if (nrow(scores) < nrow(rule$crossing)) {
    warning("Missing some results for ", rule$id, call. = FALSE)
  }

  rule$assessment(scores)
}

assessments <- map(rules, assess_rule, scores=scores) %>% list_as_tibble() %>% mutate(rule_id = map_chr(rules, "id"))
assessments %>% unnest(assessment)

assessments$plot
