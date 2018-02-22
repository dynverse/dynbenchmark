library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("4-method_characterisation")


##  ............................................................................
##  QC sheet processing                                                     ####
# Download qc & initial processing
implementation_qc_sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "QualityControl")

categories <- unique(implementation_qc_sheet$category) %>% discard(is.na)
applications <- c("developer_friendly", "user_friendly", "good_science")

implementation_qc_converted <- implementation_qc_sheet %>%
  filter(active) %>%
  tidyr::fill(aspect_id) %>%
  mutate(aspect_id = factor(aspect_id, levels = implementation_qc_sheet$aspect_id %>% unique() %>% keep(~!is.na(.)))) %>%
  group_by(aspect_id) %>%
  tidyr::fill(name, category, weight, aspect, !!applications, references) %>%
  mutate_at(vars(!!applications), ~ifelse(is.na(.), FALSE, TRUE)) %>%
  ungroup()

implementation_cols <- colnames(implementation_qc_converted)[seq(which(colnames(implementation_qc_converted) == "item")+1, ncol(implementation_qc_converted))]

implementation_qc_converted <- implementation_qc_converted %>% mutate(check_id = row_number())

# create checks
checks <- implementation_qc_converted[, colnames(implementation_qc_converted) %>% keep(~!(. %in% implementation_cols))]

implementation_qc_molten <- implementation_qc_converted %>%
  gather("implementation_id", "answer", !!implementation_cols)

# process answer, NA coercion warnings are normal here due to check of
implementation_qc_processed <- implementation_qc_molten %>%
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
implementation_qc_processed %>% group_by(implementation_id) %>% summarise(answered = all(!is.na(answer))) %>% ggplot() + geom_point(aes(answered, implementation_id))

implementation_qc_processed <- implementation_qc_processed %>%
  group_by(implementation_id) %>%
  filter(!any(is.na(answer))) %>%
  ungroup()

implementation_qc_processed$answer <- ifelse(is.na(implementation_qc_processed$answer), 0, implementation_qc_processed$answer)

implementation_qc <- implementation_qc_processed

write_rds(implementation_qc_processed, derived_file("implementation_qc.rds"))
write_rds(checks, derived_file("checks.rds"))

##  ............................................................................
##  Calculate final qc scores                                               ####
# calculate average category scores
implementation_qc_category_scores <- implementation_qc %>%
  group_by(implementation_id, category) %>%
  summarise(qc_score=sum(answer * item_weight * weight)/sum(item_weight * weight)) %>%
  ungroup()

# use the average category scores to calculate the final qc_score
implementation_qc_scores <- implementation_qc_category_scores %>%
  group_by(implementation_id) %>%
  summarise(qc_score=mean(qc_score)) %>%
  arrange(-qc_score) %>%
  ungroup()

# calculate the average application scores
implementation_qc_application_scores <- implementation_qc %>%
  gather(application, application_applicable, !!qc_applications$application) %>%
  filter(application_applicable) %>%
  group_by(implementation_id, application) %>%
  summarise(score=sum(answer * item_weight * weight)/sum(item_weight * weight)) %>%
  ungroup()


write_rds(implementation_qc_scores, derived_file("implementation_qc_scores.rds"))
write_rds(implementation_qc_category_scores, derived_file("implementation_qc_category_scores.rds"))
write_rds(implementation_qc_application_scores, derived_file("implementation_qc_application_scores.rds"))
