library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

# Rsync latest results!
# PRISM:::rsync_remote(
#   remote_src = "prism",
#   path_src = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
#   remote_dest = "",
#   path_dest = derived_file(),
#   verbose=T
# )

############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

olist <- read_rds(derived_file("outputs_postprocessed.rds"))

# get ordering of methods
method_ord <- olist$outputs_summtrajtype_totalsx2 %>%
  filter(task_source == "mean", trajectory_type == "overall") %>%
  arrange(desc(harm_mean)) %>%
  .$method_name

# create method_name_f factor in all data structures
for (oname in str_subset(names(olist), "outputs")) {
  olist[[oname]] <- olist[[oname]] %>%
    mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(olist, environment())
rm(olist)

# collect which methods use which prior information
prior_df <- outputs_ind %>% select(method_name, prior_str) %>% distinct()


############### OVERALL COMPARISON ###############
metr_lev <- c(
  "harm_mean", "norm_correlation", "norm_edge_flip", "norm_rf_mse",
  "real", "synthetic", "time_method", "rank_time_method",
  "pct_errored", "pct_time_exceeded", "pct_memory_exceeded", "num_setseed_calls"
)

oc1 <-
  outputs_summtrajtype_totalsx2 %>%
  filter(task_source == "mean", trajectory_type_f == "overall") %>%
  select(method_name, method_short_name, method_name_f, paramset_id, one_of(metr_lev)) %>%
  gather(metric, score, -method_name:-paramset_id)
oc2 <-
  outputs_summtrajtype_totalsx2 %>%
  filter(task_source == c("real", "synthetic"), trajectory_type == "overall") %>%
  select(method_name:paramset_id, method_name_f, metric = task_source, score = harm_mean)

overall_comp <-
  bind_rows(oc1, oc2) %>%
  mutate(metric_f = factor(metric, levels = metr_lev))

pdf(figure_file("1_overall_comparison.pdf"), 16, 12)
ggplot(overall_comp) +
  geom_hline(aes(yintercept = y), data_frame(y = .5, metric_f = factor(metr_lev[1:6], levels = metr_lev))) +
  geom_bar(aes(method_name_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", ncol = 4, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(legend.position = "none")
dev.off()

rm(overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(outputs_summtrajtype_totalsx2$method_name_f))
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(figure_file("2_trajtype_comparison.pdf"), 20, 12)
ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, harm_mean, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, norm_correlation, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, norm_edge_flip, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, norm_rf_mse, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )
dev.off()


