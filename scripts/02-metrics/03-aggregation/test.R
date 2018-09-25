methods <-
  tribble(
    ~id, ~shape, ~color,
    "A variable method", 16, "#FF851B",
    "A bad method",  15, "#FF4136",
    "A decent method", 17, "#2ECC40",
    "Another variable method", 18, "#FFDC00"
  )

scores_individual <- bind_rows(
  tibble(
    dataset_id = "simple",
    method_id = methods$id,
    score = seq(0.05, 1, length.out = nrow(methods))
  ),
  tibble(
    dataset_id = "hard",
    method_id = methods$id,
    score = {x <- seq(0.05, 0.3, length.out = nrow(methods)); c(last(x), tail(head(x, -1), -1), first(x))}
  )
)


scores_mean <- scores_individual %>%
  group_by(method_id) %>%
  summarise(score = mean(score)) %>%
  mutate(dataset_id = "mean")


scores_mean_norm <- scores_individual %>%
  group_by(dataset_id) %>%
  mutate(score = .benchmark_aggregate_normalisation$scalesigmoid(score, score)) %>%
  group_by(method_id) %>%
  summarise(score = mean(score)) %>%
  mutate(dataset_id = "norm_mean")

scores <- bind_rows(
  scores_individual,
  scores_mean,
  scores_mean_norm
) %>%
  mutate(
    dataset_id = forcats::fct_inorder(dataset_id),
    method_id = factor(method_id, levels = methods$id)
  )

scores %>%
  ggplot(aes(dataset_id, score, color = method_id, shape = method_id)) +
    geom_point(size = 4) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_shape_manual(values = methods %>% select(id, shape) %>% deframe()) +
    scale_color_manual(values = methods %>% select(id, color) %>% deframe()) +
    theme_pub() +
    theme(legend.position = "top")
