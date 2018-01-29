library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION")

source("analysis/analyses/4-method_characterisation/0_common.R")

evals <- read_rds(derived_file("eval_outputs.rds", "5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION"))
list2env(evals, environment())
method_char <- read_rds(derived_file("method_df_evaluated.rds", "method_characteristics"))
method_df_evaluated <- method_char %>%
  rowwise() %>%
  do({
    df <- .
    method_short_name <- strsplit(df$ids, ",")[[1]]
    data.frame(df, method_short_name, stringsAsFactors = FALSE, check.names = FALSE)
  })

eval_ind <- eval_ind %>% rowwise() %>% mutate(error_message = ifelse(is.null(error), "", error$message)) %>% ungroup()

num_reals <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "real", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
num_synths <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "synthetic", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
write_tsv(num_reals, figure_file("trajtypes_real.tsv"))
write_tsv(num_synths, figure_file("trajtypes_synth.tsv"))

# adding totals and means to table
eval_trajtype_wm <- eval_trajtype %>%
  group_by(method_name, method_name_f, method_short_name, param_group, trajectory_type, trajectory_type_f) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_trajtype)

eval_overall_wm <- eval_overall %>%
  group_by(method_name, method_name_f, method_short_name, param_group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_overall)

eval_trajtype_wa_wo <- eval_overall_wm %>%
  mutate(trajectory_type = "overall") %>%
  bind_rows(eval_trajtype_wm) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = c("overall", trajtype_ord)))

# plots
pdf(figure_file("2_trajtype_comparison.pdf"), 20, 8)
ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_correlation)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL,
    colour = "Parameter\ngroup",
    y = "Mean quantile score of correlation in pairwise tented geodesic distance between cells",
    title = "Evaluation of trajectory inference methods with default parameters"
  )

# # ggplot(eval_trajtype_wa_wo) +
# #   geom_point(aes(method_name_f, rank_mrsq)) +
# #   coord_flip() +
# #   theme_bw() +
# #   facet_grid(task_group~trajectory_type_f) +
# #   labs(
# #     x = NULL,
# #     colour = "Parameter\ngroup",
# #     y = "Mean quantile score of OOB-MSE score when predicting gold milestone percentages",
# #     title = "Evaluation of trajectory inference methods with default parameters"
# #   )
ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_mmse)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL,
    colour = "Parameter\ngroup",
    y = "Mean quantile score of OOB-MSE score when predicting gold milestone percentages",
    title = "Evaluation of trajectory inference methods with default parameters"
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, percentage_errored)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL,
    colour = "Parameter\ngroup",
    y = "Percentage errored",
    title = "Error rates of trajectory inference methods with default parameters"
  )
dev.off()

# pdf(figure_file("2_trajtype_comparison_best.pdf"), 12, 8)
# yy <- eval_trajtype %>%
#   filter(method_name %in% zmethord) %>%
#   group_by(method_short_name, task_group, trajectory_type_f) %>%
#   arrange(desc(rank_correlation)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   mutate(method_name_f = factor(method_name, levels = rev(zmethord)))
# ggplot(yy %>% filter(task_group == "real")) +
#   geom_point(aes(method_name_f, rank_correlation)) +
#   coord_flip() +
#   cowplot::theme_cowplot() +
#   facet_wrap(~trajectory_type_f, ncol = 3) +
#   labs(x = NULL, title = pritt("Scores on real datasets"), colour = "Parameter\ngroup")
# dev.off()
#


# thewidth <- 8
# theheight <- 6
#
# # compare best vs. default
# pdf(figure_file("best_vs_default.pdf"), thewidth, theheight)
# ggplot(joined %>% filter(task_group == "real"), aes(date, rank_correlation)) +
#   geom_smooth(span=2) +
#   geom_path(aes(group = name), linetype = "dashed") +
#   geom_point(aes(colour = param_group)) +
#   ggrepel::geom_label_repel(aes(label = name, colour = param_group)) +
#   cowplot::theme_cowplot() +
#   labs(x = "Time", y = "rank_correlation score", title = "Overall performance with optimised parameters")
# dev.off()


# multiple plots
g1 <- ggplot(method_char, aes(date, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
pdf(figure_file("qc_over_time.pdf"), thewidth, theheight)
g1
dev.off()

g2 <- ggplot(method_char, aes(date, Citations+1)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  # scale_y_log10() +
  labs(x = "Time", y = "# citations", title = "# citations over time")
pdf(figure_file("citations_over_time.pdf"), thewidth, theheight)
g2
dev.off()

mdfe <- method_char %>% mutate(
  days = difftime(Sys.time(), date, units = "days") %>% as.numeric,
  citations_per_day = Citations / days
)

pdf(figure_file("citationsperday_over_time.pdf"), thewidth, theheight)
ggplot(mdfe, aes(date, citations_per_day)) +
  geom_smooth(span = 2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "# citations per day", title = "# citations per day over time")
dev.off()

pdf(figure_file("citationsperday_over_qcscore.pdf"), thewidth, theheight)
ggplot(mdfe, aes(citations_per_day, qc_score)) +
  geom_smooth(method = "lm") +
  geom_point() +
  cowplot::theme_cowplot() +
  labs(x = "# citations per day", y = "QC score", title = "# citations per day vs. QC score")
dev.off()


g3 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(date, rank_correlation)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (rank_correlation)", title = "Overall performance with optimised parameters") +
  theme(legend.position = "bottom")
pdf(figure_file("performance_over_time.pdf"), thewidth, theheight)
g3
dev.off()

g4 <- ggplot(method_df_evaluated, aes(Citations+1, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "QC score", title = "QC score over # citations")
pdf(figure_file("qc_over_citations.pdf"), thewidth, theheight)
g4
dev.off()

g5 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(Citations+1, rank_correlation)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "Performance (rank_correlation)", title = "rank_correlation score over # citations")
pdf(figure_file("performance_over_citations.pdf"), thewidth, theheight)
g5
dev.off()

g6 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(qc_score, rank_correlation)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "QC score", y = "Score", title = "QC score over rank_correlation score")
pdf(figure_file("performance_over_qc.pdf"), thewidth, theheight)
g6
dev.off()

# best
pdf(figure_file("best_param_over_time.pdf"), thewidth, theheight)
ggplot(besttwo, aes(date, rank_correlation)) +
  geom_step(data = line, linetype = "dashed", colour = "gray") +
  geom_point() +
  # ggrepel::geom_label_repel(aes(label = name)) +
  geom_text(aes(label = name), nudge_y = .02) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (rank_correlation)") +
  scale_y_continuous(limits = c(0, max(besttwo$rank_correlation)+.05))
dev.off()


ggplot(method_df_evaluated, aes(date, qc_score, colour = trajectory_type, group = 1)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")+
  scale_colour_manual(values = trajectory_type_colors)





# error messages
error_messages_overall <-
  eval_ind %>%
  group_by(method_name, method_name_f) %>%
  mutate(num_datasets = n()) %>%
  ungroup() %>%
  filter(!sapply(error, is.null)) %>%
  rowwise() %>%
  mutate(error_message = error$message) %>%
  ungroup() %>%
  group_by(method_name, method_name_f, error_message) %>%
  summarise(num = n(), pct = num / num_datasets[[1]]) %>%
  ungroup()
error_reasons <- tribble(
  ~partial_message, ~reason,
  "reached elapsed time limit", "time limit",
  "Cannot allocate memory", "memory limit",
  "cannot open the connection", "error inside python code",
  "Column `cell_id` must be a 1d atomic vector or a list", "bug in wrapper"
)
error_reason_fun <- function(error_message) {
  greps <- sapply(error_reasons$partial_message, function(part_mess) {
    grepl(part_mess, error_message)
  })
  apply(greps, 1, function(bools) {
    if (any(bools)) {
      error_reasons$reason[bools]
    } else {
      "error in method"
    }
  })
}

error_messages_overall <- error_messages_overall %>%
  mutate(
    error_reason = error_reason_fun(error_message),
    error_reason_f = factor(error_reason, levels = names(sort(table(error_reason), decreasing = T)))
  )


pdf(figure_file("error_reasons.pdf"), 12, 6)
ggplot(error_messages_overall) +
  geom_bar(aes(method_name_f, pct, fill = error_reason_f), stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  labs(x = NULL, y = "Percentage errored", fill = "Reason")
dev.off()

tasks <- read_rds(derived_file("tasks.rds"))
tasks_dimensionality <- tasks %>% mutate(num_cells = sapply(cell_ids, length), num_genes = sapply(feature_info, nrow)) %>% select(id, num_cells, num_genes)

timeout_rates <- eval_ind %>%
  group_by(task_id, task_group) %>%
  summarise(mean_timeout = mean(grepl("time limit", error_message))) %>%
  arrange(desc(mean_timeout)) %>%
  ungroup() %>%
  left_join(tasks_dimensionality, by = c("task_id"="id"))

g1 <- ggplot(timeout_rates) + geom_point(aes(num_cells, mean_timeout, colour = task_group)) + cowplot::theme_cowplot()
g2 <- ggplot(timeout_rates) + geom_point(aes(num_genes, mean_timeout, colour = task_group)) + cowplot::theme_cowplot()
g3 <- ggplot(timeout_rates) + geom_point(aes(sqrt(num_genes*num_cells), mean_timeout, colour = task_group)) + cowplot::theme_cowplot()

pdf(figure_file("mean_timeout.pdf"), 20, 5)
cowplot::plot_grid(g1, g2, g3, nrow = 1)
dev.off()
