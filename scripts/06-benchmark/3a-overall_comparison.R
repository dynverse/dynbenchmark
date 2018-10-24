#' Overall comparison

metr_lev <- c(
  "overall", "real/gold", "real/silver", "synthetic/dyngen", "synthetic/dyntoy", "synthetic/prosstt", "synthetic/splatter",
  "norm_correlation", "norm_him", "norm_F1_branches", "norm_featureimp_wcor", "time_pred_cor", "mem_pred_cor", "progress",
  "pct_errored", "pct_execution_error", "pct_time_limit", "pct_memory_limit", "pct_method_error"
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

# display barplots for time_pred and mem_pred correlation
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
    time_pred_cor = nacor(ltime, time_lpred),
    mem_pred_cor = nacor(lmem, mem_lpred),
    progress = n() / nrow(datasets_info) / num_replicates
  ) %>%
  gather(metric, score, time_pred_cor, mem_pred_cor, progress)

overall_comp <-
  bind_rows(oc1, oc2, oc3, oc4) %>%
  filter(metric %in% metr_lev) %>%
  mutate(metric = factor(metric, levels = metr_lev))

g <-
  ggplot(overall_comp %>% mutate(method_id = fct_rev(method_id))) +
  geom_bar(aes(method_id, score, fill = metric), stat = "identity") +
  geom_text(aes(method_id, pmax(score, 0), label = method_id), overall_comp %>% filter(score < .2), nudge_y = .03, hjust = 0, colour = "#888888", size = 2.5) +
  geom_text(aes(method_id, 0, label = method_id), overall_comp %>% filter(score >= .2), nudge_y = .03, hjust = 0, colour = "white", size = 2.5) +
  facet_wrap(~metric, scales = "free_x", ncol = 7, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )
g

ggsave(result_file("1_overall_comparison.pdf"), g, width = 25, height = 15)

rm(oc1, oc2, oc3, overall_comp)

############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(data_aggregations$method_id))
# cols <- RColorBrewer::brewer.pal(8, "Dark2")
# cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(result_file("2_trajtype_comparison.pdf"), 20, 25)
for (i in seq_len(nrow(metrics_info))) {
  if (metrics_info$metric_id[[i]] %in% colnames(data_aggregations)) {
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
}
dev.off()
