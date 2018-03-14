library(tidyverse)
library(dynalysis)
library(googlesheets)

experiment("4-method_characterisation")

implementations <- read_rds(derived_file("implementations.rds"))

# combine with qc scores
implementation_qc_scores <- readRDS(derived_file("implementation_qc_scores.rds"))

# merge qc scores with implementations tibble
implementations <- implementations %>%
  select(-matches("qc_score")) %>%
  left_join(implementation_qc_scores, "implementation_id")

implementations <- implementations %>%
  mutate(evaluated = wrapper == "Done" & !is.na(wrapper))

implementations_evaluated <- implementations %>%
  filter(evaluated)

# create methods tibble
methods <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Methods")

methods <- implementations %>%
  separate_rows(method_ids) %>%
  rename(method_id = method_ids) %>%
  left_join(methods, by="method_id")
methods_evaluated <- methods %>%
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
