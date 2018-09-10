library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("07-benchmark")


############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

list2env(read_rds(result_file("benchmark_results_input.rds")), environment())
list2env(read_rds(result_file("benchmark_results_normalised.rds")), environment())

metrics_info <- dyneval::metrics %>%
  slice(match(read_rds(result_file("metrics.rds")), metric_id)) %>%
  add_row(metric_id = "overall", plotmath = "overall", latex = "\\textrm{overall}", html = "overall", long_name = "Overall score", category = "average", type = "overall", perfect = 1, worst = 0)

# get ordering of methods
method_ord <-
  data_aggregations %>%
  filter(dataset_source == "mean", dataset_trajectory_type == "overall") %>%
  arrange(desc(overall)) %>%
  select(method_id, method_name) %>%
  mutate_all(factor)

# create method_id_f factor in all data structures
data_aggregations <- data_aggregations %>% mutate(method_id = factor(method_id, levels = method_ord$method_id), method_name = factor(method_name, levels = method_ord$method_name))
data <- data %>% mutate(method_id = factor(method_id, levels = method_ord$method_id), method_name = factor(method_name, levels = method_ord$method_name))

############### OVERALL COMPARISON ###############
metr_lev <- c(
  "overall", "real", "synthetic/dyngen", "synthetic/dyntoy", "synthetic/prosstt", "synthetic/splatter",
  "correlation", "edge_flip", "him",
  "norm_correlation", "norm_edge_flip", "norm_him",
  "F1_branches", "featureimp_cor", "featureimp_wcor",
  "norm_F1_branches", "norm_featureimp_cor", "norm_featureimp_wcor",
  "rank_time",  "pct_errored", "pct_execution_error",
  "rank_mem",  "pct_time_limit", "pct_memory_limit",
  "predmem_cor", "predtime_cor"
)

# display barplots per metric
oc1 <-
  data_aggregations %>%
  filter(dataset_source == "mean", dataset_trajectory_type == "overall") %>%
  select(method_id, method_name, param_id, one_of(intersect(colnames(data_aggregations), metr_lev))) %>%
  gather(metric, score, -method_id:-param_id)

# display barplots per dataset source
oc2 <-
  data_aggregations %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id:param_id, metric = dataset_source, score = overall)

# create placeholder for dataset sources that havent run yet
oc3 <-
  data_aggregations %>%
  filter(dataset_source != "mean", dataset_trajectory_type == "overall") %>%
  select(method_id:param_id) %>%
  crossing(metric = c("real", "synthetic/dyngen", "synthetic/dyntoy", "synthetic/prosstt", "synthetic/splatter")) %>%
  mutate(score = NA) %>%
  anti_join(oc2, by = colnames(oc2) %>% setdiff("score"))

nacor <- function(x, y) {
  is_na <- is.na(x) | is.na(y)
  cor(x[!is_na], y[!is_na])
}

# display barplots for predtime and predmem correlation
oc4 <-
  data %>%
  mutate(
    time = ifelse(error_status == "memory_limit", NA, time),
    mem = ifelse(error_status == "time_limit" | mem < log10(100e6), NA, mem),
    ltime = log10(time),
    lmem = log10(mem)
  ) %>%
  group_by(method_id, method_name, param_id) %>%
  summarise(
    predtime_cor = nacor(ltime, lpredtime),
    predmem_cor = nacor(lmem, lpredmem)
  ) %>%
  gather(metric, score, predtime_cor, predmem_cor)

overall_comp <-
  bind_rows(oc1, oc2, oc3, oc4) %>%
  filter(metric %in% metr_lev) %>%
  mutate(metric = factor(metric, levels = metr_lev))

g <-
  ggplot(overall_comp %>% mutate(method_id = fct_rev(method_id))) +
  geom_bar(aes(method_id, score, fill = metric), stat = "identity") +
  geom_text(aes(method_id, pmax(score, 0), label = method_id), overall_comp %>% filter(score < .2), nudge_y = .03, hjust = 0, colour = "#888888", size = 2.5) +
  geom_text(aes(method_id, 0, label = method_id), overall_comp %>% filter(score >= .2), nudge_y = .03, hjust = 0, colour = "white", size = 2.5) +
  facet_wrap(~metric, scales = "free_x", nrow = 3, labeller = label_facet(), dir = "v") +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )
g

ggsave(result_file("1_overall_comparison.pdf"), g, width = 25, height = 15)

rm(oc1, oc2, nacor, oc3, overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(data_aggregations$method_id))
# cols <- RColorBrewer::brewer.pal(8, "Dark2")
# cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(result_file("2_trajtype_comparison.pdf"), 20, 12)
for (i in seq_len(nrow(metrics_info))) {
  g <-
    ggplot(data_aggregations) +
    geom_point(aes_string("fct_rev(method_id)", metrics_info$metric_id[[i]], colour = "method_id")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    scale_colour_manual(values = method_cols) +
    facet_grid(dataset_source ~ dataset_trajectory_type) +
    labs(
      x = NULL,
      title = paste0(metrics_info$metric_id[[i]], ": ", metrics_info$long_name[[i]])
    )
  print(g)
}
dev.off()


############### COMPARISON PREDICTED VERSUS ACTUAL TIMES / MEM USAGE ###############
join <-
  data %>%
  filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
  mutate(
    time = ifelse(error_status == "memory_limit", NA, time),
    mem = ifelse(error_status == "time_limit" | mem < log10(100e6), NA, mem),
    ltime = log10(time),
    lmem = log10(mem)
  )

range_time <- quantile(abs(join$ltime - join$lpredtime), .9, na.rm = TRUE) %>% {2 * c(-., .)}
range_mem <- quantile(abs(join$lmem - join$lpredmem), .9, na.rm = TRUE) %>% {2 * c(-., .)}

g1 <-
  ggplot(join) +
  geom_point(aes(lpredtime, ltime, colour = ltime - lpredtime)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = range_time)

g2 <-
  ggplot(join) +
  geom_point(aes(lpredmem, lmem, colour = lmem - lpredmem)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = range_mem)

pdf(result_file("compare_pred_ind.pdf"), width = 20, height = 15)
print(g1 + labs(title = "Timings"))
print(g2 + labs(title = "Memory"))
dev.off()


g1 <- ggplot(join) +
  geom_point(aes(lpredtime, ltime, colour = ltime - lpredtime)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = range_time) +
  theme(legend.position = "bottom")
g2 <- ggplot(join %>% filter(lmem > 8)) +
  geom_point(aes(lpredmem, lmem, colour = lmem - lpredmem)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = range_mem) +
  theme(legend.position = "bottom")
ggsave(result_file("compare_pred_all.pdf"), patchwork::wrap_plots(g1, g2, nrow = 1), width = 16, height = 8)

rm(join, g1, g2)



## CHECK VARIANCES PER DATASET AND METRIC
stat_funs <- c("var", "mean")
metricso <- c("overall", metrics)

dat_df <-
  data %>%
  select(method_id, dataset_id, !!metricso) %>%
  gather(metric, score, !!metricso) %>%
  group_by(dataset_id, metric) %>%
  filter(n() > 2) %>%
  rename(unnorm = score) %>%
  mutate(norm = .benchmark_aggregate_normalisation$scalesigmoid(unnorm)) %>%
  gather(type, score, unnorm, norm) %>%
  mutate(type = factor(type, levels = c("unnorm", "norm"))) %>%
  ungroup()

var_df <-
  dat_df %>%
  group_by(type, dataset_id, metric) %>%
  summarise_at(vars(score), stat_funs) %>%
  ungroup()

g <- ggplot(var_df) +
  geom_point(aes(mean, var, colour = metric)) +
  facet_wrap(~type) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()

# g
ggsave(result_file("normalisation_var_mean.pdf"), g, width = 10, height = 5)

rm(stat_funs, metricso, dat_df, var_df, g)

