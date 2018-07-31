library(tidyverse)
library(dynbenchmark)
library(googlesheets)

experiment("04-method_characterisation")

implementations_tidy <- read_rds(derived_file("implementations_tidy.rds"))
methods_tidy <- read_rds(derived_file("methods_tidy.rds"))

# combine with qc scores
implementation_qc_scores <- readRDS(derived_file("implementation_qc_scores.rds"))

# merge qc scores with implementations tibble
implementations <- implementations_tidy %>%
  select(-matches("qc_score")) %>%
  left_join(implementation_qc_scores, "implementation_id")

methods <- methods_tidy %>%
  select(-matches("qc_score")) %>%
  left_join(implementation_qc_scores, "implementation_id")

# filter evaluated
methods$evaluated <- (methods$evaluation_inclusion == "TRUE") %>% coalesce(FALSE)
implementations$evaluated <- (implementations$evaluation_inclusion == "TRUE") %>% coalesce(FALSE)

methods_evaluated <- methods %>%
  filter(evaluated)

implementations_evaluated <- implementations %>%
  filter(evaluated)

# save
write_rds(implementations, derived_file("implementations.rds"))
write_rds(implementations_evaluated, derived_file("implementations_evaluated.rds"))
write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))

write_rds(implementations, result_file("implementations.rds"))
write_rds(implementations_evaluated, result_file("implementations_evaluated.rds"))
write_rds(methods, result_file("methods.rds"))
write_rds(methods_evaluated, result_file("methods_evaluated.rds"))
