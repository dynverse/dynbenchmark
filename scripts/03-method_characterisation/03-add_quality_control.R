## Add QC scores to methods and tools tibble

library(tidyverse)
library(dynbenchmark)
library(googlesheets)

experiment("03-method_characterisation")

tools <- read_rds(derived_file("tools.rds"))
methods <- read_rds(derived_file("methods.rds"))

# combine with qc scores
tool_qc_scores <- readRDS(derived_file("tool_qc_scores.rds"))

# merge qc scores with tools tibble
tools <- tools %>%
  select(-matches("qc_score")) %>%
  left_join(tool_qc_scores, "tool_id")

methods <- methods %>%
  select(-matches("qc_score")) %>%
  left_join(tool_qc_scores, "tool_id")

# filter evaluated
methods_evaluated <- methods %>%
  filter(evaluated)

tools_evaluated <- tools %>%
  filter(evaluated)

# save
write_rds(tools, derived_file("tools.rds"))
write_rds(tools_evaluated, derived_file("tools_evaluated.rds"))
write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))
