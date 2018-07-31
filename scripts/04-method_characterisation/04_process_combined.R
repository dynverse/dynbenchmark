library(tidyverse)
library(dynbenchmark)
library(googlesheets)

experiment("04-method_characterisation")

tools_tidy <- read_rds(derived_file("tools_tidy.rds"))
methods_tidy <- read_rds(derived_file("methods_tidy.rds"))

# combine with qc scores
tool_qc_scores <- readRDS(derived_file("tool_qc_scores.rds"))

# merge qc scores with tools tibble
tools <- tools_tidy %>%
  select(-matches("qc_score")) %>%
  left_join(tool_qc_scores, "tool_id")

methods <- methods_tidy %>%
  select(-matches("qc_score")) %>%
  left_join(tool_qc_scores, "tool_id")

# filter evaluated
methods$evaluated <- (methods$evaluation_inclusion == "TRUE") %>% coalesce(FALSE)
tools$evaluated <- (tools$evaluation_inclusion == "TRUE") %>% coalesce(FALSE)

methods_evaluated <- methods %>%
  filter(evaluated)

tools_evaluated <- tools %>%
  filter(evaluated)

# save
write_rds(tools, derived_file("tools.rds"))
write_rds(tools_evaluated, derived_file("tools_evaluated.rds"))
write_rds(methods, derived_file("methods.rds"))
write_rds(methods_evaluated, derived_file("methods_evaluated.rds"))

write_rds(tools, result_file("tools.rds"))
write_rds(tools_evaluated, result_file("tools_evaluated.rds"))
write_rds(methods, result_file("methods.rds"))
write_rds(methods_evaluated, result_file("methods_evaluated.rds"))
