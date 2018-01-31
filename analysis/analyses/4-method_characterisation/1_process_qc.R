# This file processes the quality control spread sheet

library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("method_characteristics")

# Download qc & initial processing
method_qc_sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "QualityControl")

categories <- unique(method_qc_sheet$category) %>% discard(is.na)
applications <- c("developer_friendly", "user_friendly", "good_science")

method_qc_converted <- method_qc_sheet %>%
  filter(active) %>%
  tidyr::fill(question_id) %>%
  mutate(question_id = factor(question_id, levels = method_qc_sheet$question_id %>% unique() %>% keep(~!is.na(.)))) %>%
  group_by(question_id) %>%
  tidyr::fill(name, category, weight, question, !!applications, references) %>%
  mutate_at(vars(!!applications), ~ifelse(is.na(.), FALSE, TRUE)) %>%
  ungroup()

method_cols <- colnames(method_qc_converted)[seq(which(colnames(method_qc_converted) == "scoring")+1, ncol(method_qc_converted))]

method_qc_converted <- method_qc_converted %>% mutate(check_id = row_number())

# create checks
checks <- method_qc_converted[, colnames(method_qc_converted) %>% keep(~!(. %in% method_cols))]

method_qc_molten <- method_qc_converted %>%
  gather("method_id", "answer", !!method_cols)

# process answer, NA coercion warnings are normal here due to check of
method_qc_processed <- method_qc_molten %>%
  mutate(
    answer = ifelse(is.na(answer), " ", answer),
    answer_first_char = str_split_fixed(answer, " ", 2)[, 1],
    answer_description = str_split_fixed(answer, " ", 2)[, 2],
    answer = ifelse(
      answer_first_char == ">",
      1,
      ifelse(
        answer_first_char == "?",
        NA,
        ifelse(
          suppressWarnings({!is.na(as.numeric(answer_first_char))}),
          suppressWarnings(as.numeric(answer_first_char)),
          0
        )
      ))
  )

# are all questions answered?
method_qc_processed %>% group_by(method_id) %>% summarise(answered = all(!is.na(answer))) %>% ggplot() + geom_point(aes(answered, method_id))

method_qc_processed <- method_qc_processed %>%
  group_by(method_id) %>%
  filter(!any(is.na(answer))) %>%
  ungroup()

method_qc_processed$answer <- ifelse(is.na(method_qc_processed$answer), 0, method_qc_processed$answer)

saveRDS(method_qc_processed, derived_file("method_qc.rds"))
saveRDS(checks, derived_file("checks.rds"))
