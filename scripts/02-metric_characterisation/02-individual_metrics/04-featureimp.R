library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("02-metric_characterisation/02-individual_metrics")

dataset <- load_dataset("real/fibroblast-reprogramming_treutlein")

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
  geom_point(data = feature_importances %>% filter(highlighted), color = "red") +
  scale_x_discrete("Genes", breaks = feature_importances %>% filter(highlighted) %>% pull(feature_id)) +
  theme_pub()

# plot the expression of highlighted features
dimred <- dyndimred::dimred_landmark_mds(get_expression(dataset))
plot_feature_expression <- feature_importances %>%
  filter(highlighted) %>%
  mutate(feature_id = as.character(feature_id)) %>%
  pmap(function(feature_id, ...) {
    plot_dimred(
      dataset,
      plot_trajectory = FALSE,
      dimred = dimred,
      feature_oi = feature_id
    ) + theme(legend.position = "bottom")
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
    plot_feature_expression,
    ncol = 1
  ),
  nrow = 1,
  widths = c(1, 3)
)

write_rds(plot_featureimp_overview, result_file("featureimp_overview.rds"))
