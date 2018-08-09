library(dynbenchmark)
library(tidyverse)

experiment("03-metric_characterisation/01-metric_conformity")

assessments <- read_rds(derived_file("assessments.rds"))
rules <- read_rds(derived_file("rules.rds"))

assessments %>%
  left_join(rules %>% select(id, description), by = c("rule_id" = "id")) %>%
  unnest(conformity) %>%
  mutate(
    metric_id = label_metrics(metric_id, label_type = "latex") %>% paste0("$", ., "$") %>% forcats::fct_inorder(),
    conforms = kableExtra::cell_spec(
      ifelse(conforms, "\U2714", "\U2716"),
      background = ifelse(conforms, "#2ECC40", "#FF4136"),
      format = "latex"
    )
  ) %>%
  mutate(metric_id = label_metrics(metric_id, label_type = "latex") %>% paste0("$", ., "$") %>% forcats::fct_inorder()) %>%
  spread(metric_id, conforms) %>%
  knitr::kable(format = "latex") %>%
  kableExtra::kable_styling()
