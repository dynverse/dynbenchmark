# This file processes the quality control spread sheet

library(tidyverse)
library(googlesheets)
library(dynalysis)
library(cowplot)


# Process qc -------------------------
method_qc_sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "QualityControl")

categories <- unique(method_qc_sheet$category) %>% {.[!is.na(.)]}
applications <- c("developer_friendly", "user_friendly", "good_science")

method_qc_converted <- method_qc_sheet %>%
  tidyr::fill(question_id) %>%
  mutate(question_id = factor(question_id, levels = method_qc_sheet$question_id %>% unique() %>% keep(~!is.na(.)))) %>%
  group_by(question_id) %>%
  tidyr::fill(name, category, weight, question, !!applications) %>%
  mutate_at(vars(!!applications), ~ifelse(is.na(.), FALSE, TRUE)) %>%
  ungroup()

method_cols <- colnames(method_qc_converted)[seq(which(colnames(method_qc_converted) == "scoring")+1, ncol(method_qc_converted))]
method_qc_molten <- method_qc_converted %>%
  mutate(check_id = row_number()) %>%
  gather("method_id", "answer", !!method_cols)

# process answer
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
          !is.na(as.numeric(answer_first_char)),
          as.numeric(answer_first_char),
          0
        )
      ))
  )

# are all questions answerred?
method_qc_processed %>% group_by(method_id) %>% summarise(answered = all(!is.na(answer))) %>% ggplot() + geom_point(aes(answered, method_id))

final_scores <- method_qc_processed %>%
  group_by(method_id) %>%
  summarise(score=sum(answer * score * weight)/sum(score * weight)) %>%
  arrange(-score)

method_qc_processed$method_id <- factor(method_qc_processed$method_id, levels=final_scores$method_id)
final_scores$method_id <- factor(final_scores$method_id, levels=final_scores$method_id)

method_ordering_plot <- final_scores %>%
  ggplot(aes(method_id, score)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_y_continuous(expand=c(0, 0.0))

method_ordering_plot

method_qc_processed %>%
  group_by(method_id, category) %>%
  summarise(score=sum(answer * score * weight)/sum(score * weight)) %>%
  ggplot(aes(method_id, category)) + geom_raster(aes(fill=score))

method_qc_processed %>%
  gather(application, application_applicable, !!applications) %>%
  filter(application_applicable) %>%
  group_by(method_id, application) %>%
  summarise(score=sum(answer * score * weight)/sum(score * weight)) %>%
  ggplot(aes(method_id, application)) + geom_raster(aes(fill=score))


breakpoint <- function(x) {
  map_dbl(0:length(x), function(i) {
    sum(1-x[seq(1, i, length.out=i)]) +
      sum(x[seq(i, length(x), length.out=length(x)-i)])
  }) %>% which.min()
}
check_breakpoints <- method_qc_processed %>%
  reshape2::acast(check_id~method_id, value.var="answer") %>%
  {
    tibble(
      breakpoint=apply(., 1, breakpoint),
      check_id = as.integer(rownames(.))
    )
  }




question_position <- method_qc_processed %>%
  filter(method_id == "SCORPIUS") %>%
  arrange(question_id) %>%
  group_by(question_id) %>%
  summarise(
    question_width = sum(score * weight),
    name = first(name)
  ) %>%
  mutate(
    question_end = cumsum(question_width),
    question_start = lag(question_end, 1, default=0),
    question_mid = question_start + (question_end - question_start)/2
  )

check_position <- method_qc_processed %>%
  filter(method_id == "SCORPIUS") %>%
  arrange(question_id, check_id) %>%
  group_by(check_id) %>%
  summarise(
    check_width = score * weight,
    question_id = first(question_id)
  ) %>%
  left_join(question_position, by="question_id") %>%
  group_by(question_id) %>%
  mutate(
    check_start = question_start + lag(cumsum(check_width), default=0),
    check_end = question_start + cumsum(check_width)
  )


check_heatmap_plot <- method_qc_processed %>%
  left_join(check_position, by="check_id") %>%
  mutate(method_id = factor(method_id)) %>%
  mutate(answer = ifelse(is.na(answer), 0, answer)) %>%
  ggplot() +
    geom_rect(aes(xmax=check_start, xmin=check_end, ymin=as.integer(method_id), ymax=as.integer(method_id) + 1, fill=category, alpha=answer)) +
    geom_vline(aes(xintercept=check_end), data=check_position, color="#444444", alpha=0.25) +
    geom_vline(aes(xintercept=question_end), data=question_position, color="#444444") +
    scale_x_reverse("Check", breaks = question_position$question_mid, labels = question_position$name, expand = c(0, 0)) +
    scale_y_continuous("Method", breaks = seq_along(levels(method_qc_processed$method_id))+0.5, labels = levels(method_qc_processed$method_id), expand = c(0, 0)) +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

empty_x <- theme(
  axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank()
)

cowplot::plot_grid(method_ordering_plot + empty_x, check_heatmap_plot, nrow=2, align = "v", axis = "rl", rel_heights = c(0.2, 0.9))
