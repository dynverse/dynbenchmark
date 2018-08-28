library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("07-benchmark")


############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

olist <- read_rds(result_file("benchmark_results.rds"))
olist$datasets_info <- NULL

# get ordering of methods
method_ord <- olist$data_trajtype_totalsx2 %>%
  filter(dataset_source == "mean", dataset_trajectory_type == "overall") %>%
  arrange(desc(overall)) %>%
  .$method_id

# create method_id_f factor in all data structures
for (oname in str_subset(names(olist), "^data") %>% setdiff("datasets_info")) {
  olist[[oname]] <- olist[[oname]] %>%
    mutate(method_id_f = factor(method_id, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(olist, environment())
rm(olist)


############### OVERALL COMPARISON ###############
metr_lev <- c(
  "overall", "real", "synthetic/dyngen", "synthetic/dyntoy", "synthetic/prosstt", "synthetic/splatter",
  "norm_correlation", "norm_edge_flip", "norm_him", "norm_F1_branches", "norm_featureimp_cor", "norm_featureimp_ks",
   "rank_time", "rank_mem", "pct_errored", "predtime_cor", "predmem_cor",
  "pct_time_limit", "pct_memory_limit", "pct_execution_error"
)


oc1 <-
  data_trajtype_totalsx2 %>%
  filter(dataset_source == "mean", dataset_trajectory_type_f == "overall") %>%
  select(method_id, method_name, method_id_f, param_id, one_of(intersect(colnames(data_trajtype_totalsx2), metr_lev))) %>%
  gather(metric, score, -method_id:-param_id, -method_id_f)
oc2 <-
  data_trajtype_totalsx2 %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id:param_id, method_id_f, metric = dataset_source, score = overall)

nacor <- function(x, y) {
  is_na <- is.na(x) | is.na(y)
  cor(x[!is_na], y[!is_na])
}

oc3 <-
  data %>%
  mutate(
    time = ifelse(error_status == "memory_limit", NA, time),
    mem = ifelse(error_status == "time_limit" | mem < log10(100e6), NA, mem),
    ltime = log10(time),
    lmem = log10(mem)
  ) %>%
  group_by(method_id, method_name, method_id_f, param_id) %>%
  summarise(
    predtime_cor = nacor(ltime, lpredtime),
    predmem_cor = nacor(lmem, lpredmem)
  ) %>%
  gather(metric, score, predtime_cor, predmem_cor)

overall_comp <-
  bind_rows(oc1, oc2, oc3) %>%
  filter(metric %in% metr_lev) %>%
  mutate(metric_f = factor(metric, levels = metr_lev))

g <-
  ggplot(overall_comp) +
  geom_bar(aes(method_id_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", ncol = 6, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(legend.position = "none")
g

ggsave(result_file("1_overall_comparison.pdf"), g, width = 25, height = 15)

rm(oc1, oc2, nacor, oc3, overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(data_trajtype_totalsx2$method_id_f))
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(result_file("2_trajtype_comparison.pdf"), 20, 12)
ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, overall, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "Overall score"
  )

ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, norm_correlation, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "Correlation"
  )

ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, norm_edge_flip, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "Edge flip"
  )

ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, norm_featureimp_cor, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "Feature imp cor"
  )

ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, norm_F1_branches, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "F1 branches"
  )
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

g1 <-
  ggplot(join) +
  geom_point(aes(lpredtime, ltime, colour = ltime - lpredtime)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = c(-4, 4))

g2 <-
  ggplot(join) +
  geom_point(aes(lpredmem, lmem, colour = lmem - lpredmem)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = c(-4, 4))

pdf(result_file("compare_pred_ind.pdf"), width = 20, height = 15)
print(g1 + labs(title = "Timings"))
print(g2 + labs(title = "Memory"))
dev.off()


g1 <- ggplot(join) +
  geom_point(aes(lpredtime, ltime, colour = ltime - lpredtime)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = c(-4, 4)) +
  theme(legend.position = "bottom")
g2 <- ggplot(join) +
  geom_point(aes(lpredmem, lmem, colour = lmem - lpredmem)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = c(-4, 4)) +
  theme(legend.position = "bottom")
ggsave(result_file("compare_pred_all.pdf"), patchwork::wrap_plots(g1, g2, nrow = 1), width = 16, height = 8)

rm(join, g1, g2)
