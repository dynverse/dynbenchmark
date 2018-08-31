#' Downloading and processing the quality control worksheet
#' Downloads the quality control values from the google sheet and processes it into a tidy format

library(tidyverse)
library(googlesheets)
library(dynbenchmark)

experiment("03-methods")

##  ............................................................................
##  QC sheet processing                                                     ####
# Download qc & initial processing
tool_qc_sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "qc")

categories <- unique(tool_qc_sheet$category) %>% discard(is.na)
applications <- c("developer_friendly", "user_friendly", "future_proof")

tool_qc_converted <- tool_qc_sheet %>%
  filter(active) %>%
  tidyr::fill(aspect_id) %>%
  mutate(aspect_id = factor(aspect_id, levels = tool_qc_sheet$aspect_id %>% unique() %>%
                              keep(~!is.na(.)))) %>%
  group_by(aspect_id) %>%
  tidyr::fill(name, category, weight, aspect, !!applications, references) %>%
  mutate_at(vars(!!applications), ~ifelse(is.na(.), FALSE, TRUE)) %>%
  ungroup()

tool_cols <- colnames(tool_qc_converted)[seq(which(colnames(tool_qc_converted) == "item")+1, ncol(tool_qc_converted))]

tool_qc_converted <- tool_qc_converted %>% mutate(check_id = row_number())

# create checks
checks <- tool_qc_converted[, colnames(tool_qc_converted) %>% keep(~!(. %in% tool_cols))]

tool_qc_molten <- tool_qc_converted %>%
  gather("tool_id", "answer", !!tool_cols)

# process answer, NA coercion warnings are normal here due to check of
tool_qc_processed <- tool_qc_molten %>%
  mutate(
    answer = ifelse(is.na(answer), "0", answer),
    answer_first_char = str_replace(answer, "([\\d\\.]*).*", "\\1"),
    answer_description = str_replace(answer, "[\\d\\.]*(.*)", "\\1") %>% trimws()
  ) %>%
  mutate(answer = as.numeric(ifelse(answer_first_char == "", "0", answer_first_char)))

tool_qc_processed <- tool_qc_processed %>%
  group_by(tool_id) %>%
  filter(!any(is.na(answer))) %>%
  ungroup()

tool_qc_processed$answer <- ifelse(is.na(tool_qc_processed$answer), 0, tool_qc_processed$answer)

tool_qc <- tool_qc_processed

write_rds(tool_qc_processed, result_file("tool_qc.rds"))
write_rds(checks, result_file("qc_checks.rds"))

##  ............................................................................
##  Calculate final qc scores                                               ####
# calculate average category scores
tool_qc_category_scores <- tool_qc %>%
  group_by(tool_id, category) %>%
  summarise(qc_score = sum(answer * item_weight * weight)/sum(item_weight * weight)) %>%
  ungroup()

# use the average category scores to calculate the final qc_score
tool_qc_scores <- tool_qc_category_scores %>%
  group_by(tool_id) %>%
  summarise(qc_score = mean(qc_score)) %>%
  arrange(-qc_score) %>%
  ungroup()

# calculate the average application scores
tool_qc_application_scores <- tool_qc %>%
  gather(application, application_applicable, !!qc_applications$application) %>%
  filter(application_applicable) %>%
  group_by(tool_id, application) %>%
  summarise(score = sum(answer * item_weight * weight)/sum(item_weight * weight)) %>%
  ungroup()


write_rds(tool_qc_scores, result_file("tool_qc_scores.rds"))
write_rds(tool_qc_category_scores, result_file("tool_qc_category_scores.rds"))
write_rds(tool_qc_application_scores, result_file("tool_qc_application_scores.rds"))
