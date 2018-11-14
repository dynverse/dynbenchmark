#'  Characterisation of the `r dynbenchmark::label_metric("featureimp_cor")` and `r dynbenchmark::label_metric("featureimp_wcor")`

library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("02-metrics/01-metric_characterisation")

dataset <- load_dataset("real/silver/fibroblast-reprogramming_treutlein")

##  ............................................................................
##  Example                                                                 ####

# calculate feature importances
feature_importances <- dynfeature::calculate_milestone_feature_importance(dataset)
feature_importances <- feature_importances %>% filter(milestone_id == "MEF")

# highlight some features for highlighting
feature_importances <- feature_importances %>%
  mutate(highlight = row_number() %in% c(round(n() / 300), round(n()/3), round(n()/3 * 2), round(n() - n() / 10))) %>%
  mutate(feature_id = fct_inorder(feature_id))

# plot the distribution of feature importances
plot_feature_importances <- feature_importances %>%
  ggplot(aes(feature_id, importance)) +
  geom_point() +
  geom_point(data = feature_importances %>% filter(highlight), color = "red") +
  scale_x_discrete("Genes", breaks = feature_importances %>% filter(highlight) %>% pull(feature_id)) +
  theme_pub()

# plot the expression of highlighted features
dimred <- dyndimred::dimred_landmark_mds(get_expression(dataset))
plot_feature_expression <- feature_importances %>%
  filter(highlight) %>%
  mutate(feature_id = as.character(feature_id)) %>%
  pmap(function(feature_id, ...) {
    plot_dimred(
      dataset,
      plot_trajectory = FALSE,
      dimred = dimred,
      feature_oi = feature_id
    ) + theme(legend.position = "bottom") + guides(color = guide_colorbar(title.position = "bottom"))
  }) %>%
  wrap_plots(nrow = 1)

# plot the dataset itself
plot_dataset <- plot_dimred(
  dataset,
  dimred = dimred,
  plot_trajectory = FALSE,
  color_density = "grouping",
  grouping = dataset$prior_information$groups_id
) + theme(legend.position = "none")

# combine the plots
plot_featureimp_overview <- wrap_plots(
  plot_dataset,
  wrap_plots(
    plot_feature_importances,
    plot_feature_expression %>% wrap_elements(),
    ncol = 1
  ),
  nrow = 1,
  widths = c(1.5, 3)
) + plot_annotation(tag_levels = "a")

plot_featureimp_overview

write_rds(plot_featureimp_overview, result_file("featureimp_overview.rds"))
ggsave(result_file("featureimp_overview.pdf"), width = 12, height = 8)


##  ............................................................................
##  Robustness to stochasticity                                             ####
dataset_design <- crossing(
  shuffle_perc = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  num_trees = c(10, 100, 1000, 10000),
  repeat_ix = 1:100
) %>%
  mutate(seed = repeat_ix)

perturb_shuffle_cells <- dynbenchmark:::perturb_shuffle_cells

check_featureimp_cor <- function(dataset, shuffle_perc, num_trees, seed, ...) {
  perturbed <- perturb_shuffle_cells(dataset, shuffle_perc)

  start <- Sys.time()

  set.seed(seed)

  # scores <- dyneval:::compute_featureimp(
  scores <- dyneval::calculate_featureimp_cor(
    dataset = dataset,
    prediction = perturbed,
    num_trees = num_trees
  )

  as_tibble(scores) %>%
    mutate(time_featureimp_cor = as.numeric( Sys.time() - start))
}

# run the experiment on qsub
qsub_config <- qsub::override_qsub_config(
  num_cores = 1,
  memory = "4G",
  batch_tasks = 25,
  wait = FALSE,
  max_wall_time = "03:00:00"
)
handle <- qsub_pmap(
  dataset_design,
  check_featureimp_cor,
  dataset = dataset,
  qsub_config = qsub_config,
  qsub_packages = c("dynbenchmark", "tidyverse"),
  qsub_environment = c("perturb_shuffle_cells")
)

save(
  handle,
  dataset,
  dataset_design,
  file = derived_file("04-featureimp.rda")
)

##
load(derived_file("04-featureimp.rda"))

scores <- qsub::qsub_retrieve(handle) %>% bind_rows()

save(
  scores,
  dataset,
  dataset_design,
  file = derived_file("04-featureimp.rda")
)
##

load(derived_file("04-featureimp.rda"))

results <- bind_cols(
  bind_rows(scores),
  dataset_design
) %>%
  mutate_at(c("shuffle_perc", "num_trees"), factor)

results$time <- results$time_featureimp_cor

##  ............................................................................
##  Plot featureimp correlation vs time                                     ####
true_featureimp_cor <- results %>%
  group_by(shuffle_perc) %>%
  filter(num_trees == last(levels(num_trees))) %>%
  summarise(true_featureimp_cor = mean(featureimp_cor))

plot_featureimp_cor_distributions <- results %>%
  ggplot(aes(shuffle_perc, featureimp_cor, color = shuffle_perc)) +
  geom_hline(aes(yintercept = true_featureimp_cor, color = shuffle_perc), data = true_featureimp_cor, linetype = "dotted") +
  geom_boxplot(aes(), outlier.shape = NA) +
  scale_y_continuous(label_metric("featureimp_cor", parse = TRUE), limits = c(0, 1), breaks = round(true_featureimp_cor$true_featureimp_cor, 2)) +
  scale_x_discrete(label_long("shuffle_perc"), labels = function(x) scales::percent(as.numeric(x))) +
  scale_color_viridis_d(label_long("shuffle_perc"), labels = function(x) scales::percent(as.numeric(x))) +
  facet_grid(.~num_trees, labeller = function(x) x %>% mutate_all(~paste0(., " trees"))) +
  theme_pub()
plot_featureimp_cor_distributions

write_rds(plot_featureimp_cor_distributions, result_file("featureimp_cor_distributions.rds"))
ggsave(result_file("featureimp_cor_distributions.rds"), width = 12, height = 8)

##  ............................................................................
##  Weighted featureimp                                                     ####

dataset_imp <- dynfeature::calculate_overall_feature_importance(dataset)

shuffle_importances <- function(importances, shuffle_perc = 1, direction = "top", ...) {
  direction <- ifelse(direction == "top", 1, -1)

  n <- round(nrow(importances) * shuffle_perc)

  chosen_ones <- importances %>% top_n(n, direction * importance) %>% pull(feature_id)

  mapper <- set_names(importances$feature_id, importances$feature_id)
  mapper[match(chosen_ones, mapper)] <- sample(chosen_ones)

  importances$feature_id <- mapper[importances$feature_id]

  importances
}

design <- crossing(
  shuffle_perc = seq(0, 1, 0.1),
  direction = "top",
  repeat_ix = 1:50
)

design$pred_imp <- pmap(design, shuffle_importances, importances = dataset_imp)

scores <- map_df(design$pred_imp, dyneval:::.calculate_featureimp_cor, dataset_imp = dataset_imp)

results <- bind_cols(design, scores) %>%
  mutate(shuffle_perc = factor(shuffle_perc))

plot_featureimp_wcor_effect <- results %>%
  gather("metric_id", "score", names(scores)) %>%
  ggplot(aes(shuffle_perc, score, color = metric_id)) +
  geom_boxplot() +
  scale_x_discrete(label_long("shuffle_perc"), labels = function(x) {scales::percent(as.numeric(x))}) +
  scale_color_discrete(label = label_metrics(names(scores), parse = TRUE)) +
  # facet_grid(~metric_id, labeller = label_facet(label_metrics, parse = TRUE)) +
  theme_pub()

write_rds(plot_featureimp_wcor_effect, result_file("featureimp_wcor_effect.rds"))
ggsave(plot_featureimp_wcor_effect, result_file("featureimp_wcor_effect.pdf"), width = 8, height = 5)
