library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")

############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
tasks_info <- map_df(
  paste0(local_tasks_folder, "/", task_ids, ".rds"),
  function(task_id) {
    read_rds(task_id) %>%
      mutate(nrow = nrow(expression[[1]]), ncol = ncol(expression[[1]])) %>%
      select(task_id = id, nrow, ncol)
  }
)

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

oi <-
  outputs_ind %>%
  left_join(tasks_info, by = "task_id")

timeout_lines <- oi %>%
  group_by(method_short_name) %>%
  summarise(max = max(time_method)) %>%
  filter(max > .5 * timeout_per_execution) %>%
  mutate(timeout_per_execution)

ct <- oi %>% filter(method_short_name == "ctvem", pct_errored == 0)
ct %>% select(time_method, nrow, ncol) %>% mutate()

gen_plot <- function(x, xlab) {
  ggplot() +
    geom_point(aes_string(x, "time_method", colour = "ifelse(pct_errored == 1, \"Yes\", \"No\")"), oi) +
    geom_hline(aes(yintercept = timeout_per_execution), timeout_lines) +
    scale_x_continuous(trans = 'log10',
                       breaks = scales::trans_breaks('log10', function(x) 10^x),
                       labels = scales::trans_format('log10', scales::math_format(10^.x))) +
    scale_y_continuous(trans = 'log10',
                       breaks = scales::trans_breaks('log10', function(x) 10^x),
                       labels = scales::trans_format('log10', scales::math_format(10^.x))) +
    theme_bw() +
    facet_wrap(~method_short_name, scales = "free") +
    scale_color_brewer(palette = "Dark2") +
    labs(x = xlab, y = "Execution time (s)", colour = "Has method errored?")
}

pdf(figure_file("dimensionality_versus_timings.pdf"), 20, 20)
gen_plot("nrow * ncol", "Num genes × num cells")

gen_plot("nrow", "Num cells")

gen_plot("ncol", "Num genes")

dev.off()
