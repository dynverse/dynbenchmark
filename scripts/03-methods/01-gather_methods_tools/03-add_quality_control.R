## Add QC scores to methods and tools tibble

library(tidyverse)
library(dynbenchmark)
library(googlesheets)

experiment("03-methods")

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

# check that all non-control evaluated methods & tools have a QC!
methods_evaluated_nonqc <- methods_evaluated %>% filter(type == "algorithm" & is.na(qc_score))
if (nrow(methods_evaluated_nonqc) != 0) {
  stop("Methods ", methods_evaluated_nonqc$id %>% glue::glue_collapse(", "), " dont have a QC score")
}

# save
write_rds(tools, derived_file("tools.rds"))
write_rds(tools_evaluated, derived_file("tools_evaluated.rds"))
write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))
