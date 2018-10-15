#'  Characterisation of the `r dynbenchmark::label_metric("isomorphic")`, `r dynbenchmark::label_metric("edge_flip")` and `r dynbenchmark::label_metric("him")`

library(dynbenchmark)
library(tidyverse)
library(furrr)

experiment("02-metrics/01-metric_characterisation")

# use all topology based metrics
metric_ids <- dyneval::metrics %>% filter(category %in% "topology") %>% pull(metric_id)

##  ............................................................................
##  Compare topologies                                                      ####
plan(multiprocess)

# generate datasets
dataset_design <- enframe(dynbenchmark:::topologies_with_same_n_milestones, "model_id", "model") %>% mutate(model_id = forcats::fct_inorder(model_id))

dataset_design$dataset <- pmap(dataset_design, function(model, ...) {
  dataset <- dyntoy::generate_trajectory(model = model %>% mutate(length = 1, directed = TRUE))
  dataset
})

# check all combinations of datasets
design <- crossing(dataset_design, dataset_design)

# calculate the scores
scores <- future_map2(design$dataset, design$dataset1, calculate_metrics, metrics = metric_ids)
scores <- scores %>% bind_rows()

results <- bind_cols(design %>% select_if(is.atomic), scores) %>%
  gather("metric_id", "score", metric_ids)

# plot datasets
plot_datasets <- map2(dataset_design$model_id, dataset_design$dataset, function(title, model) {
  dynplot::plot_topology(model) +
    ggtitle(title) +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(expand = c(0.2, 0)) +
    scale_y_continuous(expand = c(0.2, 0))
}) %>% invoke(patchwork::wrap_plots, ., nrow = 1)

# plot scores
plot_scores <- ggplot(results, aes(model_id, model_id1)) +
  geom_raster(aes(fill = score)) +
  geom_text(aes(label = round(score, 2), color = score < 0.2)) +
  facet_grid(~metric_id, labeller = label_facet(label_metrics, format = "plotmath", parse = TRUE)) +
  viridis::scale_fill_viridis("Score", option = "A", direction = 1, begin = 0.05) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = FALSE) +
  scale_x_discrete("", labels = label_long, position = "bottom", expand = c(0, 0)) +
  scale_y_discrete("", labels = label_long, expand = c(0, 0), limits = rev(levels(results$model_id))) +
  theme(axis.text.x.bottom = element_text(hjust = 1, angle = 45)) +
  coord_equal()

##  ............................................................................
##  Compare lengths in topologies                                           ####

# create milestone networks with "extra" edges
topology <- tribble(
  ~from, ~to,
  "A", "B",
  "B", "D",
  "B", "C",
  "D", "E",
  "D", "F"
) %>% mutate(directed = TRUE)
milestone_networks <- map(c(0, 0.05, 0.5, 1), function(length) {
  topology %>%
    mutate(length = ifelse(row_number() %in% c(4, 5), length, 1)) %>%
    filter(length > 0)
})
dataset_design <- tibble(milestone_network = milestone_networks)
dataset_design$perturbation <- pmap(dataset_design, function(milestone_network, ...) {
  set.seed(0)
  dataset <- dyntoy::generate_trajectory(model = milestone_network, allow_tented_progressions = FALSE, num_cells = 1000)
  dataset$milestone_percentages <- dataset$milestone_percentages %>%
    group_by(cell_id) %>%
    top_n(1, percentage) %>%
    ungroup() %>%
    mutate(percentage = 1)
  dataset$progressions <- dataset$progressions %>%
    mutate(percentage = ifelse(percentage > 0.5, 1, 0))
  dataset
})
dataset_design$dataset <- list(dataset_design$perturbation[[1]]) # map(seq_len(nrow(dataset_design)), ~)
dataset_design$perturbation_id <- c("Reference", "Very short extra edges", "Short extra edges", "Long extra edges")

# calculate the scores
scores <- future_map2(dataset_design$dataset, dataset_design$perturbation, calculate_metrics, metrics = metric_ids)
scores <- scores %>% bind_rows()

milestones <- tibble(milestone_id = dataset_design$perturbation %>% last() %>% .$milestone_ids) %>% dynplot:::add_milestone_coloring()
plot_length_datasets <- dataset_design %>%
  pmap(function(perturbation_id, perturbation, ...) {
    plot_dendro(perturbation, milestones = milestones, y_offset = 0) + ggtitle(perturbation_id)
  }) %>%
  patchwork::wrap_plots(nrow = 1)
plot_length_scores <- scores %>%
  select(!!metric_ids) %>%
  mapdf(function(scores) {
    plot <- enframe(scores, "metric_id", "score") %>%
      mutate(score = unlist(score)) %>%
      ggplot(aes(1, metric_id)) +
      geom_text(aes(label = round(score, 2))) +
      theme_pub() +
      scale_y_discrete("", labels = function(x) label_metrics(x, format = "plotmath")) +
      theme(axis.line = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  }) %>%
  {
    .[[1]] <- .[[1]] + theme(axis.line.y = element_line(), axis.text.y = element_text())
    .
  } %>%
  patchwork::wrap_plots(nrow = 1)
plot_length_scores

plot_topology_lengths <- patchwork::wrap_plots(
  plot_length_datasets,
  plot_length_scores,
  ncol = 1
)


##  ............................................................................
##  Combine plots                                                           ####
library(patchwork)
plot_topology_scores_overview <- patchwork::wrap_plots(
  plot_datasets %>% wrap_elements(),
  plot_scores %>% wrap_elements(),
  plot_topology_lengths %>% wrap_elements(),
  ncol = 1,
  heights = c(1, 2.5, 2)
) + plot_annotation(tag_levels = "a")

plot_topology_scores_overview

write_rds(plot_topology_scores_overview, result_file("topology_scores_overview.rds"))
