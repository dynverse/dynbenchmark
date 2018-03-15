library(cowplot)
library(tidyverse)
library(dynalysis)

experiment("11-evaluation_robustness")

scores <- c("harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse")

methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
outputs_ind <- outputs_list$outputs_ind %>%
  mutate(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id)
method_scores <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id) %>%
  filter(task_source=="mean")
indrep_scores <- outputs_list$outputs_summrepl %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id)
method_order <- method_scores %>%
  arrange(-harm_mean) %>%
  filter(method_id %in% methods$method_id) %>%
  pull(method_id)
method_names <- set_names(methods$method_name, methods$method_id)[method_order]


##  ............................................................................
##  Dataset variability                                                     ####
ymax <- max(indrep_scores$harm_mean)
performance_datasets_variability <- indrep_scores %>%
  mutate(method_id = factor(method_id, (method_order))) %>%
  arrange(runif(n())) %>%
  ggplot(aes(method_id, harm_mean)) +
  ggbeeswarm::geom_quasirandom(aes(color=task_source)) +
  # geom_boxplot(outlier.size=0.5) +
  geom_point(aes(color=task_source), data=method_scores, size=4, alpha=0.8) +
  # geom_point(data=method_scores, shape=15, size=4) +
  scale_y_continuous(label_long("overall_performance_on_dataset"), expand=c(0, 0.01)) +
  scale_x_discrete("", label=method_names) +
  scale_color_manual(label_long("task_source"), values=c(mean="black", synthetic="#39CCCC", real="#FF4136"), label=label_long) +
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.position = "bottom", legend.justification = "center")
performance_datasets_variability


##  ............................................................................
##  Significant performance                                                 ####
performance_variability_tests <- combn(method_order, 2) %>% t %>% as_data_frame() %>% magrittr::set_colnames(c("from", "to")) %>%
  mutate(test = map2(from, to, function(from, to) {
    wilcox.test(indrep_scores$harm_mean[indrep_scores$method_id == to], indrep_scores$harm_mean[indrep_scores$method_id == from], paired = T, conf.int=T, alternative="less")
  })) %>%
  mutate(
    p_value = map_dbl(test, "p.value"),
    q_value = p.adjust(p_value, "BH"),
    log_q_value = log10(q_value),
    estimate = map_dbl(test, "estimate"),
    effect = ifelse(estimate > 0,  "↗", "↘")
  ) %>%
  mutate(
    from = factor(from, method_order),
    to = factor(to, method_order)
  )


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Significance heatmap                                                    ####
performance_variability_tests %>%
  mutate(from = factor(from, rev(levels(from)))) %>%
  ggplot() +
  geom_tile(aes(to, from, fill=q_value), color="white") +
  geom_text(aes(to, from, label=label_pvalue(q_value))) +
  # geom_tile(aes(to, from, alpha = ifelse(q_value < 0.05, 1, 0)), size=1, color="black") +
  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(6, "YlGnBu"), values=c(0, 0.0001, 0.001, 0.01, 0.1, 1)) +
  scale_alpha_identity() +
  scale_y_discrete(labels=method_names) +
  coord_equal()



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Significance lines                                                      ####
first_significant <- performance_variability_tests %>%
  group_by(from) %>%
  filter(q_value < 0.05) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(ystart = as.numeric(from), yend = as.numeric(to)) %>%
  mutate(ystep = ystart - yend) %>%
  mutate(x=0) %>%
  arrange(-ystep)

for (i in seq_len(nrow(first_significant))) {
  row <- extract_row_to_list(first_significant, i)
  overlapping <- first_significant$ystart < row$yend & row$ystart < first_significant$yend
  x <- which(!((1:1000 %in% first_significant$x[overlapping]))) %>% first()
  first_significant$x[i] <- x
}

first_significant <- first_significant %>%
  gather("y_pos", "y_value", ystart, yend) %>%
  arrange(as.numeric(to), -as.numeric(from)) %>%
  group_by(y_value) %>%
  mutate(y_value2 = y_value+0.1 * (row_number()-1) - (n()-1) * 0.1 / 2) %>%
  # mutate(y_value2 = y_value) %>%
  ungroup() %>%
  select(-y_value) %>%
  spread(y_pos, y_value2)

first_significant_bars <- first_significant %>%
  ggplot() +
  geom_segment(aes(x=x-0.5, xend=0, y=ystart, yend=ystart), color="#BBBBBB") + # bottom left
  geom_segment(aes(x=x-0.5, xend=0, y=yend, yend=yend), color="#BBBBBB") + # bottom right
  geom_segment(aes(x=x, xend=x, y=ystart, yend=yend), color="#FFFFFF", size=4) + # white horizontal bar
  geom_segment(aes(x=x, xend=x, y=ystart, yend=yend), color="#777777") + # actual horizontal bar
  geom_segment(aes(x=x, xend=x-0.5, y=ystart, yend=ystart), color="#777777") + # bottom left
  geom_segment(aes(x=x, xend=x-0.5, y=yend, yend=yend), color="#777777") + # bottom right
  scale_y_continuous("", breaks=seq_along(method_order), expand=c(0,0),limits=c(0.5, length(method_order)+0.5)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))
first_significant_bars



##  ............................................................................
##  Combined dataset variability and significance plot                      ####
performance_dataset_variability_combined <- plot_grid(
  first_significant_bars + theme_void(),
  performance_datasets_variability,
  ncol=1,
  align="v",
  axis="lr",
  rel_heights = c(0.2, 0.8)
)
performance_dataset_variability_combined
save_plot(figure_file("performance_dataset_variability_combined.svg"), performance_dataset_variability_combined, base_width=20, base_height=5)
write_rds(performance_dataset_variability_combined, figure_file("performance_dataset_variability_combined.rds"))


##  ............................................................................
##  Subsample datasets                                                      ####
rank_methods <- function(outputs_ind) {
  outputs_summrepl <- outputs_ind %>%
    group_by(method_name, method_short_name, task_id, paramset_id, trajectory_type, task_source, prior_str, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(
      harm_mean = apply(cbind(rank_correlation, rank_edge_flip, rank_rf_mse), 1, psych::harmonic.mean)
    )

  outputs_summtrajtype <- outputs_summrepl %>%
    group_by(method_name, method_short_name, task_source, paramset_id, trajectory_type, trajectory_type_f) %>%
    mutate(n = n()) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup()

  outputs_summmethod <- outputs_summtrajtype %>%
    group_by(method_name, method_short_name, task_source, paramset_id) %>%
    mutate(n = n()) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup()

    outputs_summmethod %>%
      group_by(method_name, method_short_name, paramset_id) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_source = "mean") %>%
      mutate(method_id = method_short_name)
}


# subsample datasets, calculate harm_mean
all_task_ids <- unique(outputs_ind$task_id)

n_samples <- 100
task_sample_scores <- pbapply::pblapply(cl=8, 1:length(all_task_ids), function(n_tasks) {
  map(1:n_samples, function(tasks_sample_i) {
    task_ids <- sample(all_task_ids, n_tasks)
    method_scores <- rank_methods(outputs_ind %>% filter(task_id %in% task_ids)) %>%
      mutate(n_tasks = n_tasks, tasks_sample_i = tasks_sample_i)
  }) %>% bind_rows()
}) %>% bind_rows()

# plot harm mean correlations across subsamples
task_sample_correlations <- task_sample_scores %>%
  group_by(n_tasks, tasks_sample_i) %>%
  slice(match(method_scores$method_id, method_id)) %>%
  do(cor = cor(.$harm_mean, method_scores$harm_mean, method="spearman")) %>%
  ungroup() %>%
  mutate(cor = unlist(cor)) %>%
  mutate(n_tasks_perc = n_tasks / length(all_task_ids))
write_rds(task_sample_correlations, result_file("task_sample_correlations.rds"))

task_sample_correlations_boxplot <- task_sample_correlations %>%
  ggplot(aes(n_tasks, cor, group=n_tasks)) +
    geom_boxplot() +
    geom_hline(yintercept=1) +
    scale_y_continuous(label_long("correlation_between_method_ordering"), expand=c(0, 0), limits=c(0,1)) +
    scale_x_continuous(label_long("n_datasets"), expand=c(0, 0), breaks=c(1, seq(10, length(all_task_ids)-10, 10), length(all_task_ids)))
task_sample_correlations_boxplot

# calculate rank variability of each method
task_sample_ranks <- task_sample_scores %>%
  group_by(n_tasks, tasks_sample_i) %>%
  mutate(harm_mean_rank = rank(-harm_mean)) %>%
  ungroup()

task_sample_ranks_match <- task_sample_ranks %>%
  group_by(method_id, n_tasks) %>%
  summarise(rank_match = mean(harm_mean_rank == match(method_id, method_order)))
write_rds(task_sample_ranks_match, result_file("task_sample_ranks_match.rds"))

task_sample_ranks_match %>%
  ggplot() +
    geom_line(aes(n_tasks, rank_match, color=method_id))

# calculate n times a method is in the top
task_sample_n_top <- task_sample_ranks %>%
  group_by(n_tasks, method_id) %>%
  summarise(n_top = sum(harm_mean_rank == 1)) %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  ungroup()

task_sample_n_top %>%
  filter(n_tasks <= 10) %>%
  ggplot(aes(method_id, n_top)) +
    geom_bar(stat="identity")

# calculate the maximal subset of datasets at which a method is top
# this is based on subsampling, see beneath for a smarter method
task_sample_max_tasks_top <- task_sample_n_top %>%
  filter(n_top > 1) %>%
  group_by(method_id) %>%
  summarise(max_n_tasks_top = max(n_tasks)) %>%
  complete(method_id, fill=list(max_n_tasks_top=0))



##  ............................................................................
##  Subset of datasets for which method is top                              ####
indrep_scores <- indrep_scores %>%
  group_by(task_id) %>%
  mutate(harm_mean_rank = rank(-harm_mean)) %>%
  ungroup()

binary_scorer <- function(method_id) {
  function(include_tasks) {
    if(sum(include_tasks) == 0) {
      length(method_order)
    } else {
      rank_methods(outputs_ind %>% filter(task_id %in% all_task_ids[include_tasks == 1])) %>%
        mutate(harm_mean_rank = rank(-harm_mean)) %>%
        filter(method_id == !!method_id) %>%
        pull(harm_mean_rank)
    }
  }
}

task_sample_max_tasks_top <- pbapply::pblapply(cl=8, method_scores$method_id, function(method_id) {
  task_removal_order <- indrep_scores %>%
    filter(method_id == !!method_id) %>%
    arrange(-harm_mean_rank) %>%
    pull(task_id)

  scorer <- binary_scorer(method_id)

  include_tasks <- rep(TRUE, length(all_task_ids)) %>% set_names(all_task_ids)
  for(remove_task_id in task_removal_order) {
    include_tasks[remove_task_id] <- FALSE

    rank <- scorer(include_tasks)

    if(rank == 1) {
      break
    }
  }
  tibble(method_id = method_id, max_n_tasks_top=sum(include_tasks), task_ids = list(names(include_tasks)[include_tasks]))
}) %>% bind_rows()

task_sample_max_tasks_top <- task_sample_max_tasks_top %>% bind_rows()

write_rds(task_sample_max_tasks_top, result_file("task_sample_max_tasks_top.rds"))

task_sample_max_tasks_top %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  ggplot(aes(method_id, max_n_tasks_top)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=max_n_tasks_top), vjust=0)


# GA
# include_tasks <- rep(TRUE, length(all_task_ids))
# result <- GA::ga("binary", binary_scorer("mfa"), nBits=length(all_task_ids), maxiter=10, popSize=1000)




##  ............................................................................
##  Dataset technology influence                                            ####
method_scores_real <- outputs_list$outputs_summmethod_totals %>%
  filter(task_source == "real") %>%
  rename(method_id = method_short_name)
method_scores_technologies <- map(unique(tasks$technology) %>% discard(is.na), function(technology_id) {
  method_ranking <- outputs_ind %>%
    filter(task_id %in% tasks$task_id[tasks$technology == technology_id]) %>%
    rank_methods() %>%
    mutate(technology_id = technology_id)
}) %>% bind_rows()
method_scores_technologies <- method_scores_technologies %>%
  select(method_id, technology_id, harm_mean) %>%
  rename(harm_mean_technology = harm_mean) %>%
  left_join(method_scores_real %>% rename(harm_mean_overall = harm_mean), by="method_id")

method_scores_technologies_cors <- method_scores_technologies %>%
  group_by(technology_id) %>%
  summarise(cor=cor(harm_mean_technology, harm_mean_overall))



method_scores_technologies %>%
  ggplot(aes(harm_mean_overall, harm_mean_technology)) +
    geom_point() +
    facet_wrap(~technology_id) +
    geom_abline(intercept = 0, slope = 1) +
    geom_text(aes(label=round(cor, 2)), x=0.1, y=max(method_scores_technologies$harm_mean_overall), data=method_scores_technologies_cors, size=5) +
    coord_equal() +
  theme_bw() +
  theme(
    legend.position="none", panel.grid = element_blank()
  )
