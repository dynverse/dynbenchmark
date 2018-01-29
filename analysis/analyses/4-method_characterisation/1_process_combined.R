library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

source("analysis/analyses/4-method_characterisation/0_common.R")

method_df <- read_rds(derived_file("method_df.rds"))

# combine with qc scores
method_qc <- readRDS(derived_file("method_qc.rds"))
method_qc_scores <- method_qc %>%
  group_by(method_id) %>%
  summarise(qc_score=sum(answer * score * weight)/sum(score * weight)) %>%
  arrange(-qc_score)

method_qc_category_scores <- method_qc %>%
  group_by(method_id, category) %>%
  summarise(qc_score=sum(answer * score * weight)/sum(score * weight)) %>%
  ungroup()

method_qc_application_scores <- method_qc %>%
  gather(application, application_applicable, !!applications) %>%
  filter(application_applicable) %>%
  group_by(method_id, application) %>%
  summarise(score=sum(answer * score * weight)/sum(score * weight))

method_df <- method_df %>%
  left_join(method_qc_scores, c("name"="method_id"))

method_df <- method_df %>%
  mutate(evaluated = wrapper == "Done")

method_df_evaluated <- method_df %>%
  filter(evaluated)

write_rds(method_df, derived_file("methods.rds"))
write_rds(method_df_evaluated, derived_file("methods_evaluated.rds"))
write_rds(method_qc_scores, derived_file("method_qc_scores.rds"))
write_rds(method_qc_category_scores, derived_file("method_qc_category_scores.rds"))
write_rds(method_qc_application_scores, derived_file("method_qc_application_scores.rds"))
