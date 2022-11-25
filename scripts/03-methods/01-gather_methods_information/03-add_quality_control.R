#' Add QC scores to methods and tools tibble

library(tidyverse)
library(dynbenchmark)
library(googlesheets)

experiment("03-methods")

tools <- read_rds(result_file("tools.rds"))
methods <- read_rds(result_file("methods.rds"))

# combine with qc scores
tool_qc_scores <- readRDS(result_file("tool_qc_scores.rds"))

# merge qc scores with tools tibble
tools <- tools %>%
  select(-tidyselect::matches("qc_score")) %>%
  left_join(tool_qc_scores, by = "tool_id")

methods <- methods %>%
  select(-tidyselect::matches("qc_score")) %>%
  left_join(tool_qc_scores, by = c("method_tool_id" = "tool_id"))

# filter evaluated
methods_evaluated <- methods %>%
  filter(method_evaluated)

tools_evaluated <- tools %>%
  filter(method_evaluated)

# check that all non-control evaluated methods & tools have a QC!
methods_evaluated_nonqc <- methods_evaluated %>% filter(method_source == "tool" & is.na(qc_score))
if (nrow(methods_evaluated_nonqc) != 0) {
  stop("Methods ", methods_evaluated_nonqc$id %>% glue::glue_collapse(", "), " dont have a QC score")
}

# save
write_rds(tools, result_file("tools.rds"))
write_rds(tools_evaluated, result_file("tools_evaluated.rds"))
write_rds(methods, result_file("methods.rds"))
write_rds(methods_evaluated, result_file("methods_evaluated.rds"))
