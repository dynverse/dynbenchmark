library(dynalysis)
library(tidyverse)
library(dynplot)

source("analysis/analyses/14-example_predictions/common_functions.R")

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

methods <- methods %>% filter(type == "algorithm")

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
ti_methods <- dynwrap::get_ti_methods()

top_methods <- methods %>% filter(directed_linear) %>% pull(method_short_name)

task_ids <- outputs_ind %>%
  filter(task_source == "real", repeat_i == 2) %>%
  filter(method_short_name %in% methods$method_id) %>%
  group_by(task_id) %>%
  mutate(rank = rank(-correlation, ties.method="min")) %>%
  ungroup() %>%
  filter(trajectory_type == "directed_linear", method_short_name == "embeddr", rank==1) %>%
  pull(task_id)

task_ids
task_ids <- "real/dentate-gyrus-neurogenesis_hochgerner"


##  .............................................................................
##  Load outputs and task                                                    ####
# load models
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

# Select outputs
methods_oi <- c("slngsht", "scorpius", "embeddr", "wndrlst", "waterfll")
outputs_plot <- outputs_oi %>%
  filter(task_id == !!task_id) %>%
  arrange(edge_flip) %>%
  slice(match(methods_oi, method_short_name)) %>%
  mutate(plot_fun = slice(ti_methods, match(method_short_name, ti_methods$short_name)) %>% pull(plot_fun))

# Replace model id with name of method
outputs_plot$model <- map2(outputs_plot$model, outputs_plot$method_short_name, function(model, method_short_name) {
  model$id <- methods$method_name[match(method_short_name, methods$method_id)]
  model
})

# No title theme
no_title_theme <- theme(legend.position = "none", plot.title = element_blank())

# Dimred of task
dimred_task <- dynplot:::check_or_perform_dimred(task, FALSE, color_by="Set3")

# Use colors of dimred also in the groups
groups <- tibble(
  group_id = task$milestone_ids
) %>%
  mutate(
    color = dimred_task$space_milestones %>% slice(match(group_id, milestone_id)) %>% pull(colour)
  ) %>%
  mutate(color = ifelse(startsWith(group_id, "INTERMEDIATE"), "grey", color))


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Default plots                                                           ####

task_default_plot <- plot_default(dimred_task, line_size = 2, arrow_length = unit(0.3, "cm")) +
  no_title_theme

outputs_plot$default_plot <- map(outputs_plot$model, function(prediction) {
  dimred_prediction <- dynplot:::check_or_perform_dimred(prediction, FALSE)
  dimred_prediction$space_samples$colour <- dimred_task$space_samples %>% slice(match(dimred_prediction$space_samples$cell_id, cell_id)) %>% pull(colour)
  dimred_prediction$space_samples <- dimred_prediction$space_samples %>% sample_n(nrow(dimred_prediction$space_samples)) # shuffle cells, so that no prior z-order is maintained

  prediction_plot <- plot_default(dimred_prediction, line_size = 2, arrow_length = unit(0, "cm"), label="none", plot_milestones=F) +
    no_title_theme

  prediction_plot
})

cowplot::plot_grid(plotlist = c(list(task_default_plot), outputs_plot$default_plot), nrow=1)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Linearised plots                                                        ####
plot_linearised <- function(model, label=FALSE) {
  cell_linearisation <- dynplot:::linearize_cells(model$milestone_network, model$progressions, margin=0, one_edge = TRUE)
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
    scale_x_continuous("Pseudotime", breaks = c(0, 1)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

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

  plot
}

grouping_assignment <- task$cell_grouping
group_order <- unique(c(task$milestone_network$from, task$milestone_network$to))

task_linearised_plot <- plot_linearised(task, label=TRUE)

outputs_plot$linearised_plot <- map(outputs_plot$model, plot_linearised) %>%
  map(~.)

cowplot::plot_grid(plotlist = c(list(task_linearised_plot), outputs_plot$linearised_plot), nrow=1)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Method plots                                                            ####
## Dataset plot
task_method_plot <- plot_task_cells(task)

# Method plots
outputs_plot <- outputs_plot %>%
  mutate(method_plot = map2(model, plot_fun, function(model, plot_fun) {
    print(model$id)
    plot_fun(model) + theme(legend.position = "none", plot.title = element_blank())
  }))
outputs_plot$method_plot %>% c(list(task_method_plot), .) %>% cowplot::plot_grid(plotlist=., nrow=1)


# Titles
task_title_plot <- ggplot() + geom_text(aes(0, 0, label="Gold standard"), size = 6, fontface = "bold", hjust = .5) + theme_void()
outputs_plot$title_plot <- map(outputs_plot$method_short_name, function(method_short_name) {
  title <- methods$method_name[match(method_short_name, methods$method_id)]
  ggplot() + geom_text(aes(0, 0, label=title), size = 6, fontface = "bold", hjust = .5) + theme_void()
})

row_labels <- list(
  ggplot() + theme_void(),
  ggplot() + geom_text(aes(0, 0, label="Comparison to\ngold standard")) + theme_void(),
  ggplot() + geom_text(aes(0, 0, label="Method's own\nplotting function")) + theme_void()
)

c(
  row_labels[1],
  list(task_title_plot),
  outputs_plot$title_plot,
  row_labels[2],
  list(task_linearised_plot),
  outputs_plot$linearised_plot,
  row_labels[3],
  list(task_method_plot),
  outputs_plot$method_plot
) %>%
  cowplot::plot_grid(plotlist=., nrow=3, rel_heights = c(1, 5, 5), rel_widths=c(1, rep(2, length(task_title_plot))))
