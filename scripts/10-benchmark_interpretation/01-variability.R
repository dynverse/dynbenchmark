#' The variability of the benchmarking results across dataset sources and trajectory types

library(tidyverse)
library(dynbenchmark)

experiment("10-benchmark_interpretation")

methods <- load_methods()
data <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))$data %>% filter(method_id %in% methods$id)
dataset_weights <- data %>%
  distinct(dataset_id, dataset_trajectory_type, dataset_source) %>%
  get_dataset_weighting()

get_overall_score <- function(data) {
  out <- benchmark_aggregate(data)

  out$data_aggregations %>%
    filter(dataset_trajectory_type == "overall", dataset_source == "mean") %>%
    select(method_id, overall)
}

data_mean <- get_overall_score(data) %>%
  arrange(overall)
method_order <- data_mean %>% arrange(-overall) %>% pull(method_id)


##  ............................................................................
##  Dataset source variability                                              ####
data_oi <- data %>%
  left_join(dataset_weights, "dataset_id")

dataset_sources <- tibble(
  dataset_source = unique(data_oi$dataset_source),
  dataset_origin = gsub("([^/]*)/.*", "\\1", dataset_source)
) %>%
  group_by(dataset_origin) %>%
  mutate(
    color = RColorBrewer::brewer.pal(n() + 2, c(real = "YlOrBr", synthetic = "PuBu")[first(dataset_origin)]) %>% head(-1) %>% tail(-1)
  ) %>%
  ungroup()
scale_colour_dataset_source <- scale_colour_manual(label_long("dataset_source"), values = dataset_sources %>% select(dataset_source, color) %>% deframe())

# get overall score and filter on top methods
data_oi <- data_oi %>%
  select(method_id, overall, weight, dataset_trajectory_type, dataset_source)

plot_variability_dataset_source <- data_oi %>%
  ggplot(aes(factor(method_id, !!method_order), overall)) +
  ggbeeswarm::geom_quasirandom(aes(color = dataset_source), shape = 16, size = 1) +
  geom_point(data = data_mean, size = 7, shape = 45, color = "black") +
  scale_colour_dataset_source +
  scale_x_discrete("", labels = label_method) +
  scale_y_continuous("Overall score", expand = c(0, 0), limits = c(0, 1)) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", legend.justification = "center", panel.grid.major.y = element_line(size = 0.5, color = "#DDDDDD")) +
  guides(color = guide_legend(nrow = 1))

plot_variability_dataset_source

ggsave(result_file("variability_dataset_source.pdf"), plot_variability_dataset_source, width = 14, height = 5)
write_rds(plot_variability_dataset_source %>% patchwork::wrap_plots(), derived_file("variability_dataset_source.rds"))


##  ............................................................................
##  Dataset variability                                                     ####
scale_colour_trajectory_type <- scale_fill_manual(label_long("trajectory_type"), values = dynwrap::trajectory_types %>% select(id, colour) %>% deframe(), labels = label_long)
trajectory_types_all <- dynwrap::trajectory_types$id
trajectory_types_oi <- trajectory_types_all

data_oi <- data %>%
  left_join(dataset_weights, "dataset_id") %>%
  mutate(method_id = factor(method_id, method_order))

bw <- 0.05
densities <-
  data_oi %>%
  group_by(method_id, dataset_trajectory_type) %>%
  summarise(density = list(density(overall, bw = bw, from = 0, to = 1, n = 100))) %>%
  mutate(x = map(density, "x"), y = map(density, "y")) %>%
  unnest(x, y) %>%
  ungroup()

densities_stacked <-
  densities %>%
  group_by(method_id, x) %>%
  mutate(dataset_trajectory_type = factor(dataset_trajectory_type, trajectory_types_all)) %>% # set order of trajectory types
  arrange(dataset_trajectory_type) %>%
  mutate(norm = sum(y), y = y * y, y = y / sum(y) * norm, y = ifelse(is.na(y), 0, y)) %>% # normalise between 0 and 1
  mutate(ymax = cumsum(y), ymin = lag(ymax, default = 0)) %>%
  ungroup() %>%
  group_by(method_id) %>%
  mutate(ymin = ymin / max(ymax), ymax = ymax / max(ymax)) %>% # normalise so that the maximal density is 1
  ungroup()

densities_violin <-
  densities_stacked %>%
  group_by(method_id, x) %>%
  mutate(ymax_violin = ymax - max(ymax)/2, ymin_violin = ymin - max(ymax)/2) %>%
  ungroup()

plot_variability_trajectory_type <-
  ggplot(densities_violin) +
    geom_ribbon(
      aes(
        x,
        ymin = ymin_violin + as.numeric(method_id),
        ymax = ymax_violin + as.numeric(method_id),
        fill = dataset_trajectory_type,
        group = paste0(method_id, dataset_trajectory_type),
        alpha = dataset_trajectory_type %in% !!trajectory_types_oi
      ), position = "identity"
    ) +
    geom_point(aes(y = match(method_id, !!method_order), x = overall), data = data_mean, size = 7, shape = 45, color = "black") +
    scale_colour_trajectory_type +
    scale_y_continuous(NULL, breaks = seq_along(method_order), labels = label_method(method_order), expand = c(0, 0)) +
    scale_x_continuous("Overall score", limits = c(0, 1), expand = c(0, 0)) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.2)) +
    coord_flip() +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", legend.justification = "center") +
    guides(fill = guide_legend(nrow = 1), alpha = FALSE)

plot_variability_trajectory_type

ggsave(result_file("variability_trajectory_type.pdf"), plot_variability_trajectory_type, width = 14, height = 5)
write_rds(plot_variability_trajectory_type, derived_file("variability_trajectory_type.rds"))




dat_test <-
  data_oi %>%
  select(method_id, dataset_id, dataset_trajectory_type, overall) %>%
  inner_join(densities_violin, by = c("method_id", "dataset_trajectory_type")) %>%
  group_by(method_id, dataset_id) %>%
  mutate(
    prob1 = dnorm(x, mean = overall, sd = .025),
    prob1 = prob1 / sum(prob1),
    prob2 = y / sum(y),
    prob = prob1 * prob2
  ) %>%
  sample_n(1, weight = prob) %>%
  mutate(ysam = runif(1, ymin_violin, ymax_violin)) %>%
  ungroup()


g <-
  ggplot(dat_test) +
  geom_ribbon(
    aes(
      x,
      ymin = ymin_violin + as.numeric(method_id),
      ymax = ymax_violin + as.numeric(method_id),
      fill = dataset_trajectory_type,
      group = paste0(method_id, dataset_trajectory_type)
    ),
    position = "identity",
    alpha = .1
  ) +
  geom_point(aes(x, ysam + as.numeric(method_id), colour = dataset_trajectory_type), size = .5) +
  scale_colour_manual(label_long("trajectory_type"), values = dynwrap::trajectory_types %>% select(id, colour) %>% deframe(), labels = label_long) +
  scale_fill_manual(label_long("trajectory_type"), values = dynwrap::trajectory_types %>% select(id, colour) %>% deframe(), labels = label_long) +
  coord_flip() +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.justification = "center") +
  guides(fill = guide_legend(position = "top", nrow = 1), alpha = FALSE) +
  scale_y_continuous(NULL, breaks = seq_along(method_order), labels = label_method(method_order), expand = c(0, 0)) +
  scale_x_continuous("Overall score", limits = c(0, 1), expand = c(0, 0))

ggsave(result_file("variability_trajectory_type_points.pdf"), g, width = 14, height = 5)
