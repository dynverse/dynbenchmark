library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("14-example_predictions")

source("analysis/analyses/14-example_predictions/common_functions.R")

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

methods <- methods %>% filter(type == "algorithm")

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
descriptions <- dynmethods::get_descriptions()

top_methods <- methods %>% filter(bifurcation) %>% pull(method_short_name)

task_ids <- outputs_ind %>%
  filter(task_source == "real") %>%
  filter(trajectory_type == "bifurcation", method_short_name == "tscan", edge_flip == 1, repeat_i == 1) %>%
  pull(task_id)

task_ids
task_ids <- "real/fibroblast-reprogramming_treutlein"

##  .............................................................................
##  Load models                                                             ####
outputs_oi <- outputs_ind %>%
  filter(
    task_id %in% task_ids,
    repeat_i == 1,
    method_short_name %in% c(top_methods)
  )
outputs_oi$model <- get_models(outputs_oi)
outputs_oi <- outputs_oi %>% filter(!map_dbl(model, is.null))


##  ............................................................................
##  Get task of interest                                                    ####
task_id <- task_ids[[1]]
task <- extract_row_to_list(tasks, which(tasks$id == task_id))
task <- preprocess_task(task)


##  ............................................................................
##  Select outputs to plot                                                  ####
methods_oi <- c("slngsht", "tscan", "agapt", "ctmaptpx", "mnclddr")
outputs_plot <- outputs_oi %>%
  filter(task_id == !!task_id) %>%
  arrange(edge_flip) %>%
  slice(match(methods_oi, method_short_name))

# No title theme
no_title_theme <- theme(legend.position = "none", plot.title = element_blank())

##  ............................................................................
##  Extract colors of each group                                            ####
dimred_task <- dynplot:::check_or_perform_dimred(task, FALSE, color_by="Set3")

# Use colors of dimred also in the groups
groups <- tibble(
  group_id = task$milestone_ids
) %>%
  mutate(
    color = dimred_task$space_milestones %>% slice(match(group_id, milestone_id)) %>% pull(colour)
  ) %>%
  mutate(color = ifelse(startsWith(group_id, "INTERMEDIATE"), "grey", color))


#   ____________________________________________________________________________
#   Plotting                                                                ####
##  ............................................................................
##  Default plots                                                           ####
task_default_plot <- plot_default(dimred_task, line_size = 2, arrow_length = unit(0.3, "cm"), label="leaves") +
  no_title_theme

outputs_plot$default_plot <- map2(outputs_plot$model, outputs_plot$method_short_name, function(prediction, method_short_name) {
  dimred_prediction <- dynplot:::check_or_perform_dimred(prediction, FALSE)
  dimred_prediction$space_samples$colour <- dimred_task$space_samples %>% slice(match(dimred_prediction$space_samples$cell_id, cell_id)) %>% pull(colour)
  dimred_prediction$space_samples <- dimred_prediction$space_samples %>% sample_n(nrow(dimred_prediction$space_samples)) # shuffle cells, so that no prior z-order is maintained

  prediction_plot <- plot_default(dimred_prediction, line_size = 2, arrow_length = unit(0, "cm"), label="none", plot_milestones=F) +
    no_title_theme
})

default_row_label <- ggplot() +
  geom_text(aes(0, 0, label="Comparison to\ngold standard"), hjust=1) +
  scale_x_continuous(limits=c(-2, 0), expand=c(0,0)) +
  theme_void()

default_plots <- c(list(default_row_label, task_default_plot), outputs_plot$default_plot)

cowplot::plot_grid(plotlist=default_plots, nrow=1)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Method plots                                                            ####
## Dataset plot
task_method_plot <- plot_task_cells(task)

# Method plots
outputs_plot$plot_fun <- slice(descriptions, match(outputs_plot$method_short_name, descriptions$short_name)) %>% pull(plot_fun)

outputs_plot <- outputs_plot %>%
  mutate(method_plot = map2(model, plot_fun, function(model, plot_fun) {
    print(model$id)
    plot_fun(model) + theme(legend.position = "none", plot.title = element_blank())
  }))

method_row_label <- ggplot() +
  geom_text(aes(0, 0, label="Method's \nplotting style"), hjust=1) +
  scale_x_continuous(limits=c(-2, 0), expand=c(0,0)) +
  theme_void()

method_plots <- c(list(method_row_label, task_method_plot), outputs_plot$method_plot)

cowplot::plot_grid(plotlist=method_plots, nrow=1)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Metrics                                                                 ####
metrics_oi <- dynalysis::metrics %>% slice(match(c("correlation", "rf_mse_inv", "edge_flip"), metric_id)) %>%
  mutate(y=row_number())

metrics_y_scale <- scale_y_continuous(limits=c(min(metrics_oi$y)-0.5, max(metrics_oi$y)+0.5), expand=c(0, 0))

plot_metrics <- function(metric_df) {
  ggplot(metric_df, aes(0, y)) +
    geom_rect(aes(xmin=-1, xmax=1, ymin=y-0.4, ymax=y+0.4), fill="#FFFFFF") +
    geom_text(aes(label=round(score_value, 2), color=score_value_01), hjust=0.5, fontface="bold") +
    scale_x_continuous(limits=c(-1, 1), expand=c(0,0)) +
    metrics_y_scale +
    theme_void() +
    scale_color_gradientn(colours=c("#FF4136", "#FF851B", "#55a504"), limits=c(0, 1), guide=FALSE)
}

task_metric_plot <- plot_metrics(metrics_oi %>% mutate(score_value = perfect, score_value_01=perfect))

outputs_plot$metric_plot <- outputs_plot[, c("method_short_name", metrics_oi$metric_id)] %>%
  gather(metric_id, score_value, !!metrics_oi$metric_id) %>%
  left_join(metrics_oi, "metric_id") %>%
  group_by(metric_id) %>%
  mutate(score_value_01 = (score_value - min(score_value))/(max(score_value) - min(score_value))) %>%
  ungroup() %>%
  {split(., fct_inorder(.$method_short_name))} %>%
  map(plot_metrics)

metric_row_label <- row_labels_metric_plot <- metrics_oi %>%
  ggplot() +
  geom_text(aes(0, y, label=glue::glue("{name}\n({descriptive_name})")), hjust=1, lineheight=0.8) +
  geom_segment(aes(0.1, y, xend=0.2, yend=y)) +
  scale_x_continuous(limits=c(-2, 0.2), expand=c(0,0)) +
  theme_void() +
  metrics_y_scale

metric_plots <- c(list(metric_row_label, task_metric_plot), outputs_plot$metric_plot)

cowplot::plot_grid(plotlist=metric_plots, nrow=1)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Linear                                                                  ####
plot_linearised <- function(model, label=FALSE) {
  model$milestone_network <- dynplot:::map_order(model, task)

  margin <- 0
  cell_linearisation <- dynplot:::linearize_cells(model$milestone_network, model$progressions, margin=margin, one_edge = TRUE)
  cell_linearisation$progressions$cumpercentage <- cell_linearisation$progressions$cumpercentage %>% {(. - min(.))/(max(.) - min(.))}

  plot <- left_join(grouping_assignment, cell_linearisation$progressions, "cell_id") %>%
    left_join(groups, "group_id") %>%
    mutate(group_id = factor(group_id, rev(group_order))) %>%
    ggplot(aes(cumpercentage, group_id)) +
    ggbeeswarm::geom_quasirandom(aes(color = color), groupOnX=F) +
    theme_bw() +
    theme(legend.position = "none") +
    scale_y_discrete("", labels=NULL) +
    scale_color_identity() +
    scale_x_continuous(NULL, breaks = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x=NULL,
      panel.border = element_rect(colour = "black", fill = NA),
    )
  if(label) {
    label_data <- left_join(grouping_assignment, cell_linearisation$progressions, "cell_id") %>%
      mutate(group_id = factor(group_id, group_order)) %>%
      group_by(group_id) %>%
      summarise(cumpercentage = mean(cumpercentage)) %>%
      left_join(groups, "group_id")

    plot <- plot +
      ggrepel::geom_label_repel(aes(label=group_id, fill=color), data=label_data, direction="x", force=0.) +
      scale_fill_identity()
  }

  # connections <- plot_connections(model$milestone_network, orientation = -1, margin=margin)
  # cowplot::plot_grid(plot, connections, ncol=1, rel_heights = c(4, 1))

  plot
}

grouping_assignment <- task$cell_grouping
group_order <- unique(c(task$milestone_network$from, task$milestone_network$to))

task_linearised_plot <- plot_linearised(task, label=FALSE)

outputs_plot$linearised_plot <- map(outputs_plot$model, plot_linearised)

linearised_row_label <- ggplot() +
  geom_text(aes(0, 0, label="Comparison of\ncell ordering"), hjust=1) +
  scale_x_continuous(limits=c(-2, 0), expand=c(0,0)) +
  theme_void()

linearised_plots <- c(list(linearised_row_label, task_linearised_plot), outputs_plot$linearised_plot)

cowplot::plot_grid(plotlist=linearised_plots, nrow=1)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Titles                                                                  ####
task_title_plot <- ggplot() + geom_text(aes(0, 0, label="Gold Standard"), size = 6, fontface = "bold", hjust = .5) + theme_void()
outputs_plot$title_plot <- map(outputs_plot$method_short_name, function(method_short_name) {
  title <- methods$method_name[match(method_short_name, methods$method_id)]
  ggplot() + geom_text(aes(0, 0, label=title), size = 6, fontface = "bold", hjust = .5) + theme_void()
})

title_plots <- c(list(ggplot() + theme_void(), task_title_plot), outputs_plot$title_plot)

bifurcating_example <- c(
  title_plots,
  metric_plots,
  default_plots,
  linearised_plots,
  method_plots
) %>%
  cowplot::plot_grid(plotlist=., nrow=5, rel_heights = c(1, 2, 5, 5, 5), rel_widths=c(1.5, rep(2, length(title_plots)-1)))

cowplot::save_plot(figure_file("bifurcating_example.svg"), bifurcating_example, base_height=10, base_width=16)

