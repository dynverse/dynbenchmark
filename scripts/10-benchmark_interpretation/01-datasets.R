library(tidyverse)
library(dynbenchmark)

library(furrr)
plan(multiprocess)


experiment("10-benchmark_interpretation")

list2env(read_rds(result_file("benchmark_results_input.rds", "06-benchmark")), environment())
raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", "06-benchmark"))$raw_data
data <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))$data
data_aggregations <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))$data_aggregations

aggregate <- function(raw_data) {
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
  plot_data <- out$data %>%
    mutate(
      detects = dataset_trajectory_type %in% !!trajectory_types
    ) %>%
    left_join(dataset_weights, "dataset_id") %>%
    arrange(detects)

  raw_data_relevant <- plot_data %>% filter(detects)
  overall_scores <- get_overall_score(raw_data_relevant) %>%
    top_n(n_methods, overall)
  plot_data <- plot_data %>% filter(method_id %in% overall_scores$method_id)
  method_order <- overall_scores %>% arrange(-overall) %>% pull(method_id)

  plot_data_mean <- get_overall_score(plot_data %>% filter(detects))

  plot_data <- plot_data %>%
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

##  ............................................................................
##  Find a "good-enough" subset of methods                                  ####
method_subset_preparer_range <- function(data, range = 0.05) {
  data <- data %>%
    group_by(dataset_id) %>%
    mutate(success = overall >= max(overall) * (1-range)) %>%
    ungroup()

  data
}

method_subset_preparer_overall_cutoff <- function(data, overall_cutoff = 0.75) {
  data <- data %>%
    group_by(dataset_id) %>%
    mutate(success = overall >= overall_cutoff) %>%
    ungroup()

  data
}

method_subset_preparer_top <- function(data) {
  data <- data %>%
    group_by(dataset_id) %>%
    mutate(success = overall == max(overall)) %>%
    ungroup()

  data
}

scorer <- function(method_ids, data) {
  data %>%
    filter(method_id %in% !!method_ids) %>%
    filter(success) %>%
    group_by(dataset_id) %>%
    summarise(weight = first(weight)) %>%
    pull(weight) %>%
    sum()
}


#' @example
#' data <- out$data
#' trajectory_types_oi <- "tree"

# preparer <- method_subset_preparer_top
preparer <- function(data) {method_subset_preparer_range(data, range = 0.05)}


get_top_methods <- function(data, trajectory_types_oi) {
  # filter methods:
  # - can detect at least one of the requested trajectory type(s)
  # - does not require any prior information
  relevant_method_ids <- load_methods() %>%
    filter(
      map_lgl(trajectory_types, ~any(trajectory_types_oi %in% .)),
      !requires_prior,
      TRUE
    ) %>%
    pull(id)

  # filter methods & datasets
  data <- data %>%
    filter(dataset_trajectory_type %in% !!trajectory_types_oi) %>% # filter datasets on trajectory type
    filter(method_id %in% relevant_method_ids) %>%  # filter methods on trajectory type
    complete(dataset_id, method_id, fill = list(overall = 0)) # make sure all methods have all datasets, even if this means that they get a zero

  # add weights to datasets
  dataset_weights <- datasets_info %>%
    filter(dataset_id %in% data$dataset_id) %>%
    mutate(weight = 1) %>%
    group_by(dataset_source) %>%
    mutate(weight = weight / n() * dataset_source_weights[dataset_source]) %>%
    ungroup() %>%
    group_by(dataset_trajectory_type) %>%
    mutate(weight = weight / n()) %>%
    ungroup() %>%
    mutate(weight = weight / sum(weight))

  data <- data %>% left_join(dataset_weights %>% select(dataset_id, weight), "dataset_id")

  if(nrow(data) == 0) stop("No data found!")

  all_method_ids <- unique(data$method_id)

  # prepare data specific for this scorer
  data <- preparer(data)

  # add one method step by step
  step_ix <- 1
  steps <- list()
  method_ids <- character()
  while(length(method_ids) != length(all_method_ids)) {
    print(length(method_ids))
    next_steps <- tibble(
      method_id = setdiff(all_method_ids, method_ids)
    ) %>%
      mutate(
        score = map(method_id, c, method_ids) %>% map_dbl(scorer, data = data)
      ) %>%
      arrange(-score) %>%
      mutate(step_ix = step_ix)

    chosen_method_id <- next_steps$method_id[[1]]

    method_ids <- c(method_ids,chosen_method_id )

    steps <- c(
      steps,
      list(next_steps %>% mutate(chosen = method_id == chosen_method_id))
    )

    step_ix <- step_ix + 1
  }

  steps <- bind_rows(steps)

  steps
}





##  ............................................................................
##  Plot a single combination of trajectory types                           ####
trajectory_types_oi <- c("tree")
# trajectory_types_oi <- c("linear", "bifurcation", "multifurcation", "tree")
# trajectory_types_oi <- dynwrap::trajectory_types$id
colour <- dynwrap::trajectory_types$colour[dynwrap::trajectory_types$id == last(trajectory_types_oi)]
steps <- get_top_methods(out$data, trajectory_types_oi)


relevant_steps <- steps %>%
  group_by(step_ix) %>%
  filter(any(score < 0.95)) %>%
  ungroup()

relevant_steps_labels <- relevant_steps %>%
  filter(chosen) %>%
  mutate(label = ifelse(step_ix > 1, paste0("+ ", label_wrap(label_method(method_id))), label_wrap(label_method(method_id)))) %>%
  mutate(score_start = lag(score, default = 0))

plot_complementarity_example <- relevant_steps %>%
  ggplot(aes(step_ix, score)) +
    ggbeeswarm::geom_quasirandom(aes(color = chosen), data = filter(relevant_steps, !chosen), color = "#888888", alpha = 0.5, groupOnX = TRUE) +
  ggrepel::geom_label_repel(aes(y = score, label = label), data = relevant_steps_labels, angle = 0, label.size = 0, size = 3, lineheight = 0.8, nudge_y = 0.1, direction = "y") +
    geom_point(data = relevant_steps %>% filter(chosen), color = colour) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "#333333", alpha = 0.5) +
    scale_x_continuous(label_long("n_methods"), breaks = seq_len(max(relevant_steps$step_ix)), expand = c(0, 0), limits = c(0, max(relevant_steps$step_ix) + 1)) +
    scale_y_continuous("Datasets with a top model\n(at least a performance of 95% of best model)", limits = c(0, 1.2), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, expand = c(0, 0)) +
    expand_limits(y = c(0, 0)) +
    theme_pub() +
    theme(axis.line.y = element_blank()) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 1)

plot_complementarity_example

write_rds(plot_complementarity_example, result_file("complementarity_example.rds"))





##  ............................................................................
##  Plot the complementarity of multiple combinations of trajectory_types   ####
steps_combinations <- tibble(
  trajectory_types_oi = list(
    c("cycle"),
    c("linear"),
    c("bifurcation"),
    c("multifurcation"),
    c("tree"),
    c("graph"),
    c("disconnected_graph"),
    c("linear", "bifurcation", "multifurcation", "tree"),
    dynwrap::trajectory_types$id
  )
) %>%
  mutate(
    combination_id = map_chr(trajectory_types_oi, ~if(length(.) > 1) {paste0(first(.), " \U2192 ", last(.))} else {.}) %>% fct_inorder(),
    most_complex_trajectory_type = map_chr(trajectory_types_oi, last)
  ) %>%
  mutate(steps = map(trajectory_types_oi, get_top_methods, data = out$data))


all_steps <- steps_combinations %>%
  unnest(steps)

relevant_steps <- all_steps %>%
  group_by(step_ix) %>%
  filter(any(round(score, 5) < 1)) %>%
  ungroup()

relevant_steps_labels <- relevant_steps %>%
  group_by(combination_id) %>%
  filter(chosen) %>%
  mutate(label = ifelse(step_ix > 1, paste0("+ ", label_wrap(label_method(method_id))), label_wrap(label_method(method_id)))) %>%
  mutate(score_start = lag(score, default = 0)) %>%
  ungroup()

trajectory_type_colours <- dynwrap::trajectory_types %>% select(id, colour) %>% deframe()

plot_complementarity_combinations <- relevant_steps %>%
  ggplot(aes(x = score, xend = score, y = 1, yend = 0)) +
  # ggbeeswarm::geom_quasirandom(groupOnX = F) +
  geom_rect(aes(xmin = score_start, xmax = score, ymin = 0, ymax = 1, fill = factor(step_ix)), data = relevant_steps_labels) +
  geom_hline(yintercept = 0, color = "#888888") +
  geom_hline(yintercept = 1, color = "#888888") +
  geom_segment(x = 0, xend = 0, y = 0, yend = 1, color = "#888888") +
  geom_segment(color = "black", alpha = 0.1) +
  ggrepel::geom_text_repel(
    aes(
      x = score,
      y = 1,
      label = label
    ),
    data = relevant_steps_labels %>% filter(step_ix < 3 | (score < 0.9 & step_ix < 7)),
    nudge_y = 1,
    direction = "x",
    lineheight = 0.8,
    min.segment.length = 0,
    size = 3
  ) +
  geom_point(aes(color = most_complex_trajectory_type), data = relevant_steps %>% filter(chosen)) +
  geom_segment(aes(color = most_complex_trajectory_type), data = relevant_steps %>% filter(chosen)) +
  scale_y_continuous(limits = c(0, 3), breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous("Datasets with a top model\n(at least a performance of 95% of best model)", limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, expand = c(0, 0)) +
  scale_fill_grey(label_long("n_methods"), start = 0.95, end = 0.2, limits = c(1, 2,3,4,5,6), na.value = "#333333") +
  scale_color_manual(values = trajectory_type_colours, guide = FALSE) +
  facet_grid(combination_id~., labeller = label_facet(label_short, width = 15), switch = "y") +
  theme_pub() +
  theme(
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    strip.text.y = element_text(angle = 180, vjust = 0, size = 10),
    strip.placement.y = "left",
    strip.background.y = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(ncol = 6, label.position = "right"))

plot_complementarity_combinations

write_rds(plot_complementarity_combinations, result_file("complementarity_combinations.rds"))




##  ............................................................................
##  Combined combination plot                                               ####

plot_complementarity <- patchwork::wrap_plots(
  read_rds(result_file("complementarity_example.rds")) %>% patchwork::wrap_elements(),
  read_rds(result_file("complementarity_combinations.rds")) %>% patchwork::wrap_elements(),
  nrow = 1,
  widths = c(1, 1.5)
) + patchwork::plot_annotation(tag_levels = "a")

plot_complementarity

write_rds(plot_complementarity, result_file("complementarity.rds"))
