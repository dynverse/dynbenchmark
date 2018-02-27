library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")

############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
chosen_task_source <- "mean"

trajtypes <- outputs_list$trajtypes

# aggregate over replicates
outputs_repl <- outputs_list$outputs_ind %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation, rank_edge_flip, rank_rf_mse), 1, psych::harmonic.mean)
  )

# process trajtype grouped evaluation
outputs_trajtype <- outputs_repl %>%
  group_by(method_name, method_short_name, task_source, paramset_id, trajectory_type, trajectory_type_f, repeat_i) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# process overall evaluation
outputs_method <- outputs_trajtype %>%
  group_by(method_name, method_short_name, task_source, paramset_id, repeat_i) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# adding mean per trajtype
outputs_trajtype_totals <- bind_rows(
  outputs_trajtype,
  outputs_trajtype %>%
    group_by(method_name, method_short_name, paramset_id, trajectory_type, trajectory_type_f, repeat_i) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(task_source = "mean")
)

# adding mean per method
outputs_method_totals <-
  bind_rows(
    outputs_method,
    outputs_method %>%
      group_by(method_name, method_short_name, paramset_id, repeat_i) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_source = "mean")
  )

# combine all aggregated data frames
outputs_trajtype_totalsx2 <- bind_rows(
  outputs_method_totals %>% mutate(trajectory_type = "overall"),
  outputs_trajtype_totals
) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = trajtypes$id))


outputs_list$outputs_trajtype_totalsx2 <- outputs_trajtype_totalsx2

# get ordering of methods
method_ord <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(task_source == chosen_task_source, trajectory_type == "overall") %>%
  arrange(desc(harm_mean)) %>%
  .$method_name

# create method_name_f factor in all data structures
for (oname in str_subset(names(outputs_list), "outputs")) {
  outputs_list[[oname]] <- outputs_list[[oname]] %>%
    mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(outputs_list, environment())
rm(outputs_list)




lvls <- rev(levels(outputs_summtrajtype_totalsx2$method_name_f))
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

############### COMPARISON PER TRAJECTORY TYPE ###############
pdf(figure_file("2b_trajtype_comparison_vars.pdf"), 20, 12)
ggplot(outputs_summtrajtype_totalsx2, aes(method_name_f, harm_mean)) +
  geom_bar(aes(fill = method_name_f), alpha = .2, stat = "identity") +
  geom_point(aes(colour = method_name_f), stat = "identity", outputs_trajtype_totalsx2) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  scale_fill_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2, aes(method_name_f, rank_correlation)) +
  geom_bar(aes(fill = method_name_f), alpha = .2, stat = "identity") +
  geom_point(aes(colour = method_name_f), stat = "identity", outputs_trajtype_totalsx2) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  scale_fill_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


ggplot(outputs_summtrajtype_totalsx2, aes(method_name_f, rank_edge_flip)) +
  geom_bar(aes(fill = method_name_f), alpha = .2, stat = "identity") +
  geom_point(aes(colour = method_name_f), stat = "identity", outputs_trajtype_totalsx2) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  scale_fill_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2, aes(method_name_f, rank_rf_mse)) +
  geom_bar(aes(fill = method_name_f), alpha = .2, stat = "identity") +
  geom_point(aes(colour = method_name_f), stat = "identity", outputs_trajtype_totalsx2) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  scale_fill_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


dev.off()




