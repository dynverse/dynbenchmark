library(tidyverse)
library(dynbenchmark)

experiment("02-metrics/03-aggregation")

# generate some simple method and score data
methods <-
  tribble(
    ~id, ~shape, ~color,
    "A decent method", 17, "#2ECC40",
    "A variable method", 16, "#FF851B",
    "Another variable method", 18, "#FFDC00",
    "A bad method",  15, "#FF4136"
  )

scores_individual <- bind_rows(
  tibble(
    dataset_id = "easy",
    method_id = methods$id,
    score = c(2/3, 1, 0.1, 1/3)
  ),
  tibble(
    dataset_id = "difficult",
    method_id = methods$id,
    score = c(2/3/3, 0.1/3, 1/3, 1/3/3)
  )
)

# calculate mean
scores_mean <- scores_individual %>%
  group_by(method_id) %>%
  summarise(score = mean(score)) %>%
  mutate(dataset_id = "mean")

# normalise
scores_norm <- scores_individual %>%
  group_by(dataset_id) %>%
  mutate(score = dynbenchmark:::.benchmark_aggregate_normalisation$normal(score, score)) %>%
  ungroup() %>%
  mutate(dataset_id = paste0(dataset_id, "_normalised"))

# normalised mean
scores_mean_norm <- scores_norm %>%
  group_by(method_id) %>%
  summarise(score = mean(score)) %>%
  mutate(dataset_id = "mean_normalised")

# combine all scores
scores <- bind_rows(
  scores_individual,
  scores_mean,
  scores_norm,
  scores_mean_norm
) %>%
  mutate(
    dataset_id = forcats::fct_inorder(dataset_id),
    method_id = factor(method_id, levels = methods$id)
  )

# plot the example
plot_normalisation_reasoning <- scores %>%
  ggplot(aes(dataset_id, score, color = method_id, shape = method_id)) +
    geom_point(size = 4) +
    geom_vline(xintercept = 3.5) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_discrete("", labels = label_short) +
    scale_shape_manual("", values = methods %>% select(id, shape) %>% deframe()) +
    scale_color_manual("", values = methods %>% select(id, color) %>% deframe()) +
    theme_pub() +
    guides(fill=guide_legend(ncol=2), shape=guide_legend(ncol=2)) +
    theme(legend.position = "top")

plot_normalisation_reasoning

write_rds(plot_normalisation_reasoning, result_file("normalisation_reasoning.rds"))
