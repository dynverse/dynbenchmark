library(tidyverse)
library(dynbenchmark)

library(furrr)
plan(multiprocess)


experiment("12-evaluation_robustness")

list2env(read_rds(result_file("benchmark_results_input.rds", "07-benchmark")), environment())
raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", "07-benchmark"))$raw_data
data <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))$data
data_aggregations <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))$data_aggregations

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

##  ............................................................................
##  Dataset variability                                                     ####
out <- aggregate(raw_data)

trajectory_type <- "tree"

colour <- trajectory_types$colour[trajectory_types$id == trajectory_type]

raw_data_relevant <- raw_data %>% filter(dataset_trajectory_type == !!trajectory_type)
overall_scores <- get_overall_score(raw_data_relevant)
method_order <- overall_scores %>% arrange(-overall) %>% pull(method_id)


plot_data <- out$data %>%
  mutate(
    detects = dataset_trajectory_type == !!trajectory_type,
    dataset_source_weight = dataset_source_weights[dataset_source]
  ) %>%
  arrange(detects)

plot_data_mean <- get_overall_score(plot_data %>% filter(detects))


plot_dataset_variability <- plot_data %>%
  ggplot(aes(factor(method_id, method_order), overall, size = dataset_source_weight)) +
  # geom_violin(fill = "#333333", alpha = 0.5) +
  # geom_violin(data = plot_data %>% filter(detects), fill = colour, alpha = 0.5) +
  # geom_boxplot(fill = "#333333", alpha = 0.5) +
  # geom_boxplot(data = plot_data %>% filter(detects), fill = colour, alpha = 0.5) +

  ggbeeswarm::geom_quasirandom(data = plot_data, color = "#AAAAAA", alpha = 0.5, shape = 16) +
  ggbeeswarm::geom_quasirandom(aes(color = dataset_trajectory_type)) +

  geom_point(data = plot_data_mean, size = 20, shape = 45, color = "white", alpha = 0.5) +
  geom_point(data = plot_data_mean, size = 10, shape = 45) +
  scale_x_discrete("", labels = label_method) +
  scale_y_continuous("Overall score", expand = c(0, 0), limits = c(0, 1)) +
  scale_size_continuous(range = c(0.1, 1)) +
  # scale_color_manual(label_long("detects"), values = c(`TRUE` = "orange", `FALSE` = "#333333"), label = label_long) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.justification = "center", panel.grid.major.y = element_line(size = 0.5, color = "#DDDDDD"))

plot_dataset_variability

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
#' trajectory_type <- "tree"

get_top_methods <- function(data, trajectory_types_oi) {
  # filter methods:
  # - can detect the requested trajectory type(s)
  # - does not require any prior information
  relevant_method_ids <- load_methods() %>%
    filter(
      map_lgl(trajectory_types, ~all(trajectory_types_oi %in% .)),
      !requires_prior
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
  data <- method_subset_preparer_range(data, range = 0.05)

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


# trajectory_types_oi <- c("linear", "bifurcation", "convergence", "multifurcation", "tree")
trajectory_types_oi <- c("linear")
colour <- trajectory_types$colour[trajectory_types$id == last(trajectory_types_oi)]
steps <- get_top_methods(out$data, trajectory_types_oi)


relevant_steps <- steps %>%
  group_by(step_ix) %>%
  filter(any(round(score, 5) < 1)) %>%
  ungroup()

relevant_steps_labels <- relevant_steps %>%
  filter(chosen) %>%
  mutate(label = ifelse(step_ix > 1, paste0("+ ", label_method(method_id)), label_method(method_id)))

relevant_steps %>%
  ggplot(aes(score, step_ix)) +
    ggbeeswarm::geom_quasirandom(aes(color = chosen), data = filter(relevant_steps, !chosen), groupOnX = FALSE, color = "#888888", alpha = 0.5) +
    geom_point(data = relevant_steps %>% filter(chosen), color = colour) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "#333333", alpha = 0.5) +
    geom_label(aes(x = score + 0.01, label = label), data = relevant_steps_labels, angle = 0, hjust = 0, label.size = 0) +
    scale_y_reverse(label_long("n_methods"), breaks = seq_len(max(relevant_steps$step_ix)), expand = c(0, 0.5)) +
    scale_x_continuous("Probability of getting a top model\n(which has at least a performance of 95% of best model)", limits = c(0, 1.4), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, expand = c(0.05, 0)) +
    annotate("segment", x = -Inf, xend = 1, y = Inf, yend = Inf) +
    theme_pub() +
    theme(axis.line.x = element_blank()) +
    coord_fixed(ratio = 0.05)

