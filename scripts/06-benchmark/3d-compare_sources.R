#' Compare the different dataset sources

dircom <-
  data_aggregations %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id:dataset_source, overall) %>%
  spread(dataset_source, overall)

g <-
  GGally::ggpairs(dircom %>% select(-1:-4)) +
  theme_bw()

ggsave(result_file("compare_sources.pdf"), g, width = 12, height = 12)
