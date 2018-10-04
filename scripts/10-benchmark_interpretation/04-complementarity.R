library(tidyverse)
library(dynbenchmark)

experiment("10-benchmark_interpretation")

data <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))$data

trajectory_type_colours <- dynwrap::trajectory_types %>% select(id, colour) %>% deframe()

# remove datasets on which all methods failed
data_succeeded <- data %>% group_by(dataset_id) %>% summarise(pct_succeeded = mean(error_status == "no_error")) %>% arrange(desc(pct_succeeded))
dataset_sel <- data_succeeded %>% filter(pct_succeeded > 0) %>% pull(dataset_id)
data <- data %>% filter(dataset_id %in% dataset_sel)

##  ............................................................................
##  Find a "good-enough" subset of methods                                  ####
# several functions to define when a particular method has succeeded on a particular dataset

# is it in range of the top method?
method_subset_preparer_range <- function(data_oi, range = 0.05) {
  data_oi <- data_oi %>%
    group_by(dataset_id) %>%
    mutate(success = overall >= max(overall) * (1-range)) %>%
    ungroup()

  data_oi
}

# has it a high overall score?
method_subset_preparer_overall_cutoff <- function(data_oi, overall_cutoff = 0.75) {
  data_oi <- data_oi %>%
    group_by(dataset_id) %>%
    mutate(success = overall >= overall_cutoff) %>%
    ungroup()

  data_oi
}

# does it have the top score?
method_subset_preparer_top <- function(data_oi) {
  data_oi <- data_oi %>%
    group_by(dataset_id) %>%
    mutate(success = overall == max(overall)) %>%
    ungroup()

  data_oi
}

# score a given set of methods on the datasets, by summing over the weights of the successful datasets
scorer <- function(method_ids, data_oi) {
  data_oi %>%
    filter(method_id %in% !!method_ids) %>%
    filter(success) %>%
    group_by(dataset_id) %>%
    summarise(weight = first(weight)) %>%
    pull(weight) %>%
    sum()
}


#' @example
#' trajectory_types_oi <- "tree"
#' data_oi <- data
#' preparer <- method_subset_preparer_range
get_top_methods <- function(data_oi, trajectory_types_oi, preparer = method_subset_preparer_range) {
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
  data_oi <- data_oi %>% filter(method_id %in% relevant_method_ids)

  # filter datasets
  data_oi <- data_oi %>%
    filter(dataset_trajectory_type %in% !!trajectory_types_oi) %>% # filter datasets on trajectory type
    complete(dataset_id, method_id, fill = list(overall = 0)) # make sure all methods have all datasets, even if this means that they get a zero overall score

  # add dataset weights
  dataset_weights_oi <- get_dataset_weighting(data_oi %>% distinct(dataset_id, dataset_source, dataset_trajectory_type))

  data_oi <- data_oi %>%
    left_join(dataset_weights_oi, "dataset_id")

  if (nrow(data_oi) == 0) stop("No data found!")

  all_method_ids <- unique(data_oi$method_id)

  # get the successes for this data_oi
  data_oi <- preparer(data_oi)

  # add one method step by step
  step_ix <- 1
  steps <- list()
  method_ids <- character()
  while (length(method_ids) != length(all_method_ids)) {
    print(length(method_ids))
    next_steps <- tibble(
      method_id = setdiff(all_method_ids, method_ids)
    ) %>%
      mutate(
        score = map(method_id, c, method_ids) %>% map_dbl(scorer, data_oi = data_oi)
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
##  Compute step data                                                       ####
steps_combinations <- tibble(
  trajectory_types_oi = list(
    dynwrap::trajectory_types$id,
    c("linear", "bifurcation", "multifurcation", "tree"),
    c("cycle"),
    c("linear"),
    c("bifurcation"),
    c("multifurcation"),
    c("tree"),
    c("graph"),
    c("disconnected_graph")
  )
) %>%
  mutate(
    combination_id = map_chr(trajectory_types_oi, function(trajectory_types_oi) {
      if (all(dynwrap::trajectory_types$id %in% trajectory_types_oi)) {
        "All trajectory types"
      } else if (length(trajectory_types_oi) > 1) {
        paste0(first(trajectory_types_oi), " \U2192 ", last(trajectory_types_oi))
      } else {
        trajectory_types_oi
      }
    }) %>% fct_inorder(),
    most_complex_trajectory_type = map_chr(trajectory_types_oi, last)
  ) %>%
  mutate(steps = map(trajectory_types_oi, get_top_methods, data = data))

all_steps <-
  steps_combinations %>%
  unnest(steps)


##  ............................................................................
##  Plot a single combination of trajectory types                           ####
relevant_steps_ex <-
  all_steps %>%
  filter(combination_id == "All trajectory types") %>%
  group_by(step_ix) %>%
  filter(any(score < 0.9)) %>%
  ungroup()

relevant_steps_labels_ex <-
  relevant_steps_ex %>%
  filter(chosen) %>%
  mutate(label = ifelse(step_ix > 1, paste0("+ ", label_wrap(label_method(method_id))), label_wrap(label_method(method_id)))) %>%
  mutate(score_start = lag(score, default = 0))

ymax <- max(relevant_steps_ex$step_ix)
ybreaks <- seq_len(ymax)
plot_complementarity_example <-
  ggplot(relevant_steps_ex, aes(score, -step_ix)) +
  geom_segment(aes(x = score, xend = score, y = -(step_ix + .5 + .1), yend = -(step_ix + .5 + .9), colour = most_complex_trajectory_type), data = filter(relevant_steps_ex, chosen), alpha = .5) +
  ggbeeswarm::geom_quasirandom(aes(color = chosen), data = filter(relevant_steps_ex, !chosen), color = "#888888", alpha = 0.8, size = 1, groupOnX = FALSE) +
  ggrepel::geom_label_repel(aes(x = score, label = label), data = relevant_steps_labels_ex, angle = 0, label.size = 0, size = 3, lineheight = 0.8, nudge_x = .1, direction = "x") +
  geom_point(aes(colour = most_complex_trajectory_type), data = relevant_steps_ex %>% filter(chosen)) +
  scale_x_continuous(limits = c(0, 1.1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, expand = c(0, 0)) +
  scale_y_continuous(breaks = -ybreaks, labels = ybreaks, expand = c(0, 0), limits = c(-(ymax + .5), -.5)) +
  scale_color_manual(values = trajectory_type_colours, guide = FALSE) +
  # expand_limits(x = c(0, 0)) +
  theme_pub() +
  labs(
    x = "Likelihood of obtaining a top model",
    y = label_long("n_methods")
  ) +
  lemon::coord_capped_cart(bottom = "both", left = lemon::brackets_vertical())

plot_complementarity_example

ggsave(result_file("complementarity_example.pdf"), plot_complementarity_example, width = 6, height = 6)
write_rds(plot_complementarity_example, derived_file("complementarity_example.rds"))




##  ............................................................................
##  Plot the complementarity of multiple combinations of trajectory_types   ####
comb_ypos <-
  steps_combinations %>%
  select(combination_id) %>%
  mutate(y = as.integer(combination_id), y = max(y) - y + 1)

bar_height <- .33
text_width <- .2

relevant_steps <-
  all_steps %>%
  group_by(step_ix) %>%
  filter(any(round(score, 5) < 1)) %>%
  ungroup() %>%
  left_join(comb_ypos, by = "combination_id")

relevant_steps_labels <-
  relevant_steps %>%
  group_by(combination_id) %>%
  filter(chosen) %>%
  mutate(label = ifelse(step_ix > 1, paste0("+ ", label_wrap(label_method(method_id))), label_wrap(label_method(method_id)))) %>%
  mutate(score_start = lag(score, default = 0)) %>%
  ungroup()

plot_complementarity_combinations <-
  ggplot(relevant_steps) +
  geom_rect(aes(xmin = score_start, xmax = score, ymin = y, ymax = y + bar_height, fill = factor(step_ix)), data = relevant_steps_labels) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = y, ymax = y + bar_height), comb_ypos, colour = "#888888", fill = NA) +
  geom_segment(aes(x = score, xend = score, y = y, yend = y + bar_height), color = "black", alpha = 0.1) +
  geom_point(aes(x = score, y = y, color = most_complex_trajectory_type), data = relevant_steps %>% filter(chosen)) +
  geom_text(aes(-text_width / 2, y + bar_height / 2, label = label_short(combination_id, width = 15)), comb_ypos, hjust = .5) +
  ggrepel::geom_text_repel(
    aes(
      x = score,
      y = y,
      label = label
    ),
    data = relevant_steps_labels %>% filter(step_ix < 3 | (score < 0.9 & step_ix < 7)),
    nudge_y = -(1 - bar_height)/2,
    direction = "x",
    lineheight = 0.8,
    min.segment.length = 0,
    size = 3
  ) +
  geom_segment(aes(x = score, xend = score, y = y, yend = y + bar_height, color = most_complex_trajectory_type), data = relevant_steps %>% filter(chosen)) +
  scale_y_continuous(limits = c(.5, max(comb_ypos$y) + .5), breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(limits = c(-text_width, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = scales::percent, expand = c(0, .03)) +
  scale_fill_grey(label_long("n_methods"), start = 0.95, end = 0.2, limits = c(1, 2,3,4,5,6), na.value = "#333333") +
  scale_color_manual(values = trajectory_type_colours, guide = FALSE) +
  theme_pub() +
  theme(
    axis.title.y = element_blank(),
    axis.line.y = element_blank(),
    strip.text.y = element_text(angle = 180, vjust = 1, size = 10),
    strip.placement.y = "left",
    strip.background.y = element_blank(),
    panel.spacing = unit(0, "cm"),
    legend.position = "bottom",
    legend.justification = "center"
  ) +
  guides(fill = guide_legend(ncol = 6, label.position = "right")) +
  labs(x = "Likelihood of obtaining a top model") +
  lemon::coord_capped_cart(bottom = "both")

plot_complementarity_combinations

write_rds(plot_complementarity_combinations, derived_file("complementarity_combinations.rds"))

##  ............................................................................
##  Combined complementarity plot                                           ####

plot_complementarity <- patchwork::wrap_plots(
  read_rds(derived_file("complementarity_example.rds")),
  read_rds(derived_file("complementarity_combinations.rds")),
  nrow = 1,
  widths = c(1, 1.5)
) +
  patchwork::plot_annotation(tag_levels = "a")

plot_complementarity

ggsave(result_file("complementarity.pdf"), plot_complementarity, width = 13, height = 6, device = cairo_pdf)

