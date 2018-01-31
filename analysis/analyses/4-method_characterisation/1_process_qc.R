library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("method_characteristics")


##  ............................................................................
##  QC sheet processing                                                     ####
# Download qc & initial processing
method_qc_sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "QualityControl")

categories <- unique(method_qc_sheet$category) %>% discard(is.na)
applications <- c("developer_friendly", "user_friendly", "good_science")

method_qc_converted <- method_qc_sheet %>%
  filter(active) %>%
  tidyr::fill(aspect_id) %>%
  mutate(aspect_id = factor(aspect_id, levels = method_qc_sheet$aspect_id %>% unique() %>% keep(~!is.na(.)))) %>%
  group_by(aspect_id) %>%
  tidyr::fill(name, category, weight, aspect, !!applications, references) %>%
  mutate_at(vars(!!applications), ~ifelse(is.na(.), FALSE, TRUE)) %>%
  ungroup()

method_cols <- colnames(method_qc_converted)[seq(which(colnames(method_qc_converted) == "item")+1, ncol(method_qc_converted))]

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

# are all checks answered?
method_qc_processed %>% group_by(method_id) %>% summarise(answered = all(!is.na(answer))) %>% ggplot() + geom_point(aes(answered, method_id))

method_qc_processed <- method_qc_processed %>%
  group_by(method_id) %>%
  filter(!any(is.na(answer))) %>%
  ungroup()

method_qc_processed$answer <- ifelse(is.na(method_qc_processed$answer), 0, method_qc_processed$answer)

method_qc <- method_qc_processed

write_rds(method_qc_processed, derived_file("method_qc.rds"))
write_rds(checks, derived_file("checks.rds"))

##  ............................................................................
##  Calculate final qc scores                                               ####
# calculate average category scores
method_qc_category_scores <- method_qc %>%
  group_by(method_id, category) %>%
  summarise(qc_score=sum(answer * item_weight * weight)/sum(item_weight * weight)) %>%
  ungroup()

# use the average category scores to calculate the final qc_score
method_qc_scores <- method_qc_category_scores %>%
  group_by(method_id) %>%
  summarise(qc_score=mean(qc_score)) %>%
  arrange(-qc_score)

# calculate the average application scores
method_qc_application_scores <- method_qc %>%
  gather(application, application_applicable, !!qc_applications$application) %>%
  filter(application_applicable) %>%
  group_by(method_id, application) %>%
  summarise(score=sum(answer * item_weight * weight)/sum(item_weight * weight))


write_rds(method_qc_scores, derived_file("method_qc_scores.rds"))
write_rds(method_qc_category_scores, derived_file("method_qc_category_scores.rds"))
write_rds(method_qc_application_scores, derived_file("method_qc_application_scores.rds"))
