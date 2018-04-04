library(dynalysis)
library(tidyverse)
library(dynplot)

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

# Get outputs
outputs_oi <- outputs_ind %>%
  filter(
    task_id %in% task_ids,
    repeat_i == 1,
    method_short_name %in% c(top_methods)
  )
outputs_oi$model <- pbapply::pblapply(cl=8, seq_len(nrow(outputs_oi)), function(row_i) {
  load_dyneval_model(
    method_short_name = outputs_oi$method_short_name[[row_i]],
    model_id = outputs_oi$model_id[[row_i]],
    experiment_id = "5-optimise_parameters/3-evaluate_parameters"
  )[[1]]
})
outputs_oi <- outputs_oi %>% filter(!map_dbl(model, is.null))

# Select first task
task_id <- task_ids[[1]]
print(task_id)
task <- extract_row_to_list(tasks, which(tasks$id == task_id))

# Preprocess task
task$milestone_network <- dynalysis:::cut_unrepresented_milestones(task$milestone_network, task$milestone_percentages %>% filter(percentage > 0), task$milestone_ids)
task$milestone_network <- task$milestone_network %>% mutate(from = factor(from, levels=task$milestone_ids)) %>% arrange(from) %>% mutate(from = as.character(from)) # retain order from before
task$milestone_ids <- unique(c(task$milestone_network$from, task$milestone_network$to))
task$milestone_percentages <- task$milestone_percentages %>% filter(percentage > 0)

milestone_network <- task$milestone_network
milestone_ids <- task$milestone_ids

# Select outputs
outputs_plot <- outputs_oi %>%
  filter(task_id == !!task_id) %>%
  arrange(edge_flip) %>%
  slice(match(c("slngsht", "tscan", "agapt", "ctmaptpx", "mnclddr"), method_short_name)) %>%
  mutate(plot_fun = slice(descriptions, match(method_short_name, descriptions$short_name)) %>% pull(plot_fun))

# Replace model id with name of method
outputs_plot$model <- map2(outputs_plot$model, outputs_plot$method_short_name, function(model, method_short_name) {
  model$id <- methods$method_name[match(method_short_name, methods$method_id)]
  model
})

# No title theme
no_title_theme <- theme(legend.position = "none", plot.title = element_blank())

# Dimred of task

# devtools::reload("../dynplot")
# devtools::reload("../dynmethods")
# dimred of task, to get colors
task$id <- "Gold standard"
dimred_task <- dynplot:::check_or_perform_dimred(task, FALSE, color_by="Set3")

# Use colors of dimred also in the groups
groups <- tibble(
  group_id = task$milestone_ids
) %>%
  mutate(
    color = dimred_task$space_milestones %>% slice(match(group_id, milestone_id)) %>% pull(colour)
  ) %>%
  mutate(color = ifelse(startsWith(group_id, "INTERMEDIATE"), "grey", color))

#######
## Default plots
task_default_plot <- plot_default(dimred_task, line_size = 2, arrow_length = unit(0.3, "cm")) +
  no_title_theme

outputs_plot$default_plot <- map2(outputs_plot$model, outputs_plot$method_short_name, function(prediction, method_short_name) {
  dimred_prediction <- dynplot:::check_or_perform_dimred(prediction, FALSE)
  dimred_prediction$space_samples$colour <- dimred_task$space_samples %>% slice(match(dimred_prediction$space_samples$cell_id, cell_id)) %>% pull(colour)
  dimred_prediction$space_samples <- dimred_prediction$space_samples %>% sample_n(nrow(dimred_prediction$space_samples)) # shuffle cells, so that no prior z-order is maintained

  prediction_plot <- plot_default(dimred_prediction, line_size = 2, arrow_length = unit(0, "cm"), label="none", plot_milestones=F) +
    no_title_theme
})

cowplot::plot_grid(plotlist = c(list(task_default_plot), outputs_plot$default_plot), nrow=1)




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



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Titles                                                                  ####
task_title_plot <- ggplot() + geom_text(aes(0, 0, label="Gold Standard"), size = 4, fontface = "bold", hjust = .5) + theme_void()
outputs_plot$title_plot <- map(outputs_plot$method_short_name, function(method_short_name) {
  title <- methods$method_name[match(method_short_name, methods$method_id)]
  ggplot() + geom_text(aes(0, 0, label=title), size = 4, fontface = "bold", hjust = .5) + theme_void()
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
  list(task_default_plot),
  outputs_plot$default_plot,
  row_labels[3],
  list(task_method_plot),
  outputs_plot$method_plot
) %>%
  cowplot::plot_grid(plotlist=., nrow=3, rel_heights = c(1, 5, 5), rel_widths=c(1, rep(2, length(task_title_plot))))
