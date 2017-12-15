library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

source("analysis/analyses/4-method_characterization/0_common.R")

evals <- read_rds(derived_file("eval_outputs.rds", "5-optimise_parameters/8-evaluate_with_real_datasets"))
list2env(evals, environment())
method_char <- read_rds(derived_file("method_df_evaluated.rds", "method_characteristics"))
method_df_evaluated <- method_char %>%
  rowwise() %>%
  do({
    df <- .
    method_short_name <- strsplit(df$ids, ",")[[1]]
    data.frame(df, method_short_name, stringsAsFactors = FALSE, check.names = FALSE)
  })

ggplot(eval_repl) +
  geom_point(aes(method_name_f, rank_auc_R_nx, colour = param_group)) +
  facet_wrap(~task_group, nrow = 1) +
  coord_flip() +
  cowplot::theme_cowplot()

ggplot(eval_repl) +
  geom_boxplot(aes(method_name_f, rank_auc_R_nx, colour = param_group)) +
  facet_wrap(~task_group, nrow = 1) +
  coord_flip() +
  cowplot::theme_cowplot()

ggplot(eval_overall) +
  geom_point(aes(method_name_f, rank_auc_R_nx, colour = param_group)) +
  facet_wrap(~task_group, nrow = 1) +
  coord_flip() +
  cowplot::theme_cowplot()

joined <- eval_overall %>% left_join(method_df_evaluated, by = "method_short_name") %>% filter(method_short_name != "ouija")

besttwo <- joined %>% group_by(method_short_name, task_group) %>% arrange(desc(rank_auc_R_nx)) %>% slice(1) %>% ungroup() %>% filter(task_group == "real")
line <- besttwo %>%
  filter(name %in% c("Monocle 1", "Waterfall", "embeddr", "TSCAN", "SCORPIUS", "slingshot")) %>%
  add_row(date = Sys.time(), rank_auc_R_nx = max(besttwo$rank_auc_R_nx)) %>%
  add_row(date = "2014-01-01", rank_auc_R_nx = 0) %>%
  arrange(date)



pdf(figure_file("1_overall_comparison.pdf"), 12, 4)
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(eval_overall %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, rank_auc_R_nx, colour = param_group)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()

pdf(figure_file("1_overall_comparison_best.pdf"), 12, 4)
z <- joined %>% group_by(method_short_name, task_group) %>% arrange(desc(rank_auc_R_nx)) %>% slice(1) %>% ungroup()
zmethord <- z %>% filter(task_group == "real") %>% arrange(desc(rank_auc_R_nx)) %>% .$method_name
z <- z %>% mutate(method_name_f = factor(method_name, levels = rev(zmethord)))
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(z %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, rank_auc_R_nx)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()

pdf(figure_file("2_trajtype_comparison.pdf"), 12, 16)
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(eval_trajtype %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, rank_auc_R_nx, colour = param_group)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    facet_wrap(~trajectory_type_f, ncol = 1) +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()
#


thewidth <- 8
theheight <- 6

# compare best vs. default
pdf(figure_file("best_vs_default.pdf"), thewidth, theheight)
ggplot(joined %>% filter(task_group == "real"), aes(date, rank_auc_R_nx)) +
  geom_smooth(span=2) +
  geom_path(aes(group = name), linetype = "dashed") +
  geom_point(aes(colour = param_group)) +
  ggrepel::geom_label_repel(aes(label = name, colour = param_group)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "rank_auc_R_nx score", title = "Overall performance with optimised parameters")
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


g3 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(date, rank_auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (rank_auc_R_nx)", title = "Overall performance with optimised parameters") +
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

g5 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(Citations+1, rank_auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "Performance (rank_auc_R_nx)", title = "rank_auc_R_nx score over # citations")
pdf(figure_file("performance_over_citations.pdf"), thewidth, theheight)
g5
dev.off()

g6 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(qc_score, rank_auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "QC score", y = "Score", title = "QC score over rank_auc_R_nx score")
pdf(figure_file("performance_over_qc.pdf"), thewidth, theheight)
g6
dev.off()

# best
pdf(figure_file("best_param_over_time.pdf"), thewidth, theheight)
ggplot(besttwo, aes(date, rank_auc_R_nx)) +
  geom_step(data = line, linetype = "dashed", colour = "gray") +
  geom_point() +
  # ggrepel::geom_label_repel(aes(label = name)) +
  geom_text(aes(label = name), nudge_y = .005) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (rank_auc_R_nx)") +
  scale_y_continuous(limits = c(0, max(besttwo$rank_auc_R_nx)+.01))
dev.off()


ggplot(method_df_evaluated, aes(date, qc_score, colour = trajectory_type, group = 1)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")+
  scale_colour_manual(values = trajectory_type_colors)
