library(tidyverse)
library(dynalysis)

experiment("method_characteristics")

methods <- read_rds(derived_file("methods.rds"))

# combine with qc scores
method_qc <- readRDS(derived_file("method_qc.rds"))

# merge qc scores with methods tibble
methods <- methods %>%
  select(-matches("qc_score")) %>%
  left_join(method_qc_scores, c("name"="method_id"))

methods <- methods %>%
  mutate(evaluated = wrapper == "Done" & !is.na(wrapper))

methods_evaluated <- methods %>%
  filter(evaluated)

write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))
write_rds(method_qc_scores, derived_file("method_qc_scores.rds"))
write_rds(method_qc_category_scores, derived_file("method_qc_category_scores.rds"))
write_rds(method_qc_application_scores, derived_file("method_qc_application_scores.rds"))
