library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

#### FIRST RUN THE RSYNC FROM SCRIPT 3C ####

list2env(read_rds(derived_file("outputs_postprocessed.rds")), environment())
list2env(read_rds(derived_file("config.rds")), environment())

tasks <- map_df(paste0(local_tasks_folder, "/", task_ids, ".rds"), read_rds)


# msn <- "tscan"
msn <- "mnclddr"
debug_out <- outputs_ind %>% filter(method_short_name == msn) %>% arrange(desc(rank_correlation)) %>% filter(repeat_i == 1)

i <- 4
model_id <- debug_out$model_id[[i]]
task_id <- debug_out$task_id[[i]]

model <- load_dyneval_model(method_short_name = msn, model_id = model_id)[[1]]
task <- tasks %>% filter(id == task_id) %>% extract_row_to_list(1)

wp_cells <- unique(c(model$waypoint_cells, task$waypoint_cells))
task_geo <- dynutils::compute_tented_geodesic_distances(task, wp_cells)
model_geo <- dynutils::compute_tented_geodesic_distances(model, wp_cells)

if (task$task_source != "real") {
  ggplot() + geom_jitter(aes(as.vector(task_geo), as.vector(model_geo)), width = .05, height = 0.05, alpha = .5) + cowplot::theme_cowplot()
} else {
  ggplot() + geom_violin(aes(factor(as.vector(task_geo)), as.vector(model_geo))) + cowplot::theme_cowplot()
}

cowplot::plot_grid(
  plot_default(task),
  plot_default(model),
  plot_combined(model, task),
  plot_combined(task, model),
  ncol = 2
)

plot_strip_connections(task, model)
