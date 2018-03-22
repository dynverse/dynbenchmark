library(tidyverse)
library(dynalysis)
library(googlesheets)

experiment("4-method_characterisation")

implementations <- read_rds(derived_file("implementations.rds"))
methods <- read_rds(derived_file("methods_tidy.rds"))

# combine with qc scores
implementation_qc_scores <- readRDS(derived_file("implementation_qc_scores.rds"))

# merge qc scores with implementations tibble
implementations <- implementations %>%
  select(-matches("qc_score")) %>%
  left_join(implementation_qc_scores, "implementation_id")

# merge implementations with methods
methods <- methods %>%
  left_join(implementations, "implementation_id")

methods_evaluated <- methods %>%
  filter(wrapper=="Done")

# save
write_rds(implementations, derived_file("implementations.rds"))
write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))

write_rds(implementations, result_file("implementations.rds"))
write_rds(methods, result_file("methods.rds"))
write_rds(methods_evaluated, result_file("methods_evaluated.rds"))
