library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("07-benchmark")


############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

olist <- read_rds(derived_file("benchmark_results.rds"))
olist$datasets_info <- NULL

# get ordering of methods
method_ord <- olist$data_trajtype_totalsx2 %>%
  filter(dataset_source == "mean", dataset_trajectory_type == "overall") %>%
  arrange(desc(harm_mean)) %>%
  .$method_id

# create method_id_f factor in all data structures
for (oname in str_subset(names(olist), "^data")) {
  olist[[oname]] <- olist[[oname]] %>%
    mutate(method_id_f = factor(method_id, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(olist, environment())
rm(olist)

# collect which methods use which prior information
# prior_df <- outputs_ind %>% select(method_name, prior_str) %>% distinct()


############### OVERALL COMPARISON ###############
metr_lev <- c(
  "harm_mean", "norm_correlation", "norm_edge_flip", "norm_featureimp_cor", "norm_F1_branches",
  "real", "synthetic/dyngen", "synthetic/dyntoy", "synthetic/prosstt", "synthetic/splatter",
  "time_method", "max_mem", "rank_time_method", "rank_max_mem", "pct_errored",
  "pct_time_limit", "pct_memory_limit", "pct_execution_error"
)

oc1 <-
  data_trajtype_totalsx2 %>%
  filter(dataset_source == "mean", dataset_trajectory_type_f == "overall") %>%
  select(method_id, method_name, method_id_f, param_id, one_of(metr_lev)) %>%
  gather(metric, score, -method_id:-param_id, -method_id_f)
oc2 <-
  data_trajtype_totalsx2 %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id:param_id, method_id_f, metric = dataset_source, score = harm_mean)

overall_comp <-
  bind_rows(oc1, oc2) %>%
  filter(metric %in% metr_lev) %>%
  mutate(metric_f = factor(metric, levels = metr_lev))

g <-
  ggplot(overall_comp) +
  geom_bar(aes(method_id_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", ncol = 5, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(legend.position = "none")
g

ggsave(result_file("1_overall_comparison.svg"), g, width = 20, height = 12)

rm(oc1, oc2, overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(data_trajtype_totalsx2$method_id_f))
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(result_file("2_trajtype_comparison.pdf"), 20, 12)
ggplot(data_trajtype_totalsx2) +
  geom_point(aes(method_id_f, harm_mean, colour = method_id_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(dataset_source~dataset_trajectory_type_f) +
  labs(
    x = NULL,
    title = "Harmonic mean"
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


