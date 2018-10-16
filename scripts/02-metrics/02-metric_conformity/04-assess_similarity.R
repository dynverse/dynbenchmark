#' Assess the similarity between metrics

library(tidyverse)
library(dynbenchmark)

experiment("02-metrics/02-metric_conformity")

# load scores
scores <-
  read_rds(derived_file("scores.rds"))

metric_ids <- unique(scores$metric_id)

scores_matrix <- scores %>%
  spread(metric_id, score) %>%
  select(metric_ids)

metric_similarity <- cor(scores_matrix, method = "spearman") %>%
  reshape2::melt(value.name = "correlation", varnames = c("from", "to"))

metric_similarity %>%
  ggplot(aes(from, to)) +
    geom_tile(aes(fill = correlation))
