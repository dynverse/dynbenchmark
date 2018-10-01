library(tidyverse)
library(dynbenchmark)

library(furrr)
plan(multiprocess)


experiment("10-benchmark_interpretation")

list2env(read_rds(result_file("benchmark_results_input.rds", "07-benchmark")), environment())
raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", "07-benchmark"))$raw_data
data <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))$data
data_aggregations <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))$data_aggregations

aggregate <- function(raw_data) {
  list2env(read_rds(result_file("benchmark_results_input.rds", "07-benchmark")), environment())
  out <- benchmark_aggregate(
    data = raw_data,
    metrics = metrics,
    norm_fun = norm_fun,
    mean_fun = mean_fun,
    mean_weights = mean_weights,
    dataset_source_weights = dataset_source_weights
  )
}

get_overall_score <- function(raw_data) {
  out <- aggregate(raw_data)

  out$data_aggregations %>%
    filter(dataset_trajectory_type == "overall", dataset_source == "mean") %>%
    select(method_id, overall)
}

dataset_weights <- datasets_info %>%
  mutate(weight = 1) %>%
  group_by(dataset_trajectory_type) %>%
  mutate(weight = weight / n()) %>%
  ungroup() %>%
  group_by(dataset_source) %>%
  mutate(weight = weight / n() * dataset_source_weights[dataset_source]) %>%
  ungroup() %>%
  mutate(weight = weight / sum(weight)) %>%
  select(dataset_id, weight)

out <- aggregate(raw_data)

##  ............................................................................
##  Dataset variability                                                     ####

trajectory_types <- "tree"
n_methods <- 5

trajectory_type_colours <- dynwrap::trajectory_types %>% select(id, colour) %>% deframe()

trajectory_types <- dynwrap::trajectory_types$id

plot_dataset_variability <- function(trajectory_types, n_methods = 9999999) {
  data <- out$data %>%
    mutate(
      detects = dataset_trajectory_type %in% !!trajectory_types
    ) %>%
    left_join(dataset_weights, "dataset_id") %>%
    arrange(detects)

  # get overall score and filter on top methods
  data_mean <- get_overall_score(data %>% filter(detects)) %>%
    top_n(n_methods, overall)

  data <- data %>% filter(method_id %in% data_mean$method_id)

  method_order <- data_mean %>% arrange(-overall) %>% pull(method_id)

  data <- data %>%
    select(method_id, overall, detects, weight, dataset_trajectory_type)

  base_plot <- plot_data %>%
    ggplot(aes(factor(method_id, method_order), overall)) +
    # geom_violin(fill = "#333333", alpha = 0.5) +
    # geom_violin(data = plot_data %>% filter(detects), fill = colour, alpha = 0.5) +
    # geom_boxplot(fill = "#333333", alpha = 0.5) +
    # geom_boxplot(data = plot_data %>% filter(detects), fill = colour, alpha = 0.5) +

    ggbeeswarm::geom_quasirandom(aes(size = weight), color = "#AAAAAA", alpha = 0.5, shape = 16) +
    scale_x_discrete("", labels = label_method) +
    scale_y_continuous("Overall score", expand = c(0, 0), limits = c(0, 1)) +
    scale_size_continuous(range = c(0.05, 1)) +
    scale_color_manual(values = trajectory_type_colours) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.justification = "center", panel.grid.major.y = element_line(size = 0.5, color = "#DDDDDD"))

  base_plot +
    ggbeeswarm::geom_quasirandom(aes(size = weight, color = dataset_trajectory_type), plot_data %>% filter(detects)) +
    geom_point(data = plot_data_mean, size = 20, shape = 45, color = "white", alpha = 0.5) +
    geom_point(data = plot_data_mean, size = 10, shape = 45)
}

trajectory_types_oi <- c(
  list(
    all = dynwrap::trajectory_types$id
  ),
  set_names(as.list(dynwrap::trajectory_types$id), dynwrap::trajectory_types$id)
)

plots_dataset_variability <- map(trajectory_types_oi, plot_dataset_variability)

plots_dataset_variability$all

write_rds(plots_dataset_variability, result_file("dataset_variability.rds"))
