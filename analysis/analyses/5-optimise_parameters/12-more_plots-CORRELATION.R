library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION")

source("analysis/analyses/4-method_characterization/0_common.R")

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

num_reals <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "real", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
num_synths <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "synthetic", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
write_tsv(num_reals, figure_file("trajtypes_real.tsv"))
write_tsv(num_synths, figure_file("trajtypes_synth.tsv"))


pdf(figure_file("2_trajtype_comparison.pdf"), 12, 16)
cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(eval_trajtype %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, rank_correlation, colour = param_group)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    facet_wrap(~trajectory_type_f, ncol = 1) +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
dev.off()


pdf(figure_file("2_trajtype_comparison_best.pdf"), 12, 8)
yy <- eval_trajtype %>%
  filter(method_name %in% zmethord) %>%
  group_by(method_short_name, task_group, trajectory_type_f) %>%
  arrange(desc(rank_correlation)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(method_name_f = factor(method_name, levels = rev(zmethord)))
ggplot(yy %>% filter(task_group == "real")) +
  geom_point(aes(method_name_f, rank_correlation)) +
  coord_flip() +
  cowplot::theme_cowplot() +
  facet_wrap(~trajectory_type_f, ncol = 3) +
  labs(x = NULL, title = pritt("Scores on real datasets"), colour = "Parameter\ngroup")
dev.off()
#


thewidth <- 8
theheight <- 6

# compare best vs. default
pdf(figure_file("best_vs_default.pdf"), thewidth, theheight)
ggplot(joined %>% filter(task_group == "real"), aes(date, rank_correlation)) +
  geom_smooth(span=2) +
  geom_path(aes(group = name), linetype = "dashed") +
  geom_point(aes(colour = param_group)) +
  ggrepel::geom_label_repel(aes(label = name, colour = param_group)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "rank_correlation score", title = "Overall performance with optimised parameters")
dev.off()


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

