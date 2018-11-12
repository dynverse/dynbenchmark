#' Check different benchmark component timings

library(dynbenchmark)
library(tidyverse)

experiment("06-benchmark/inspect_identity_timings")

raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", experiment_id = "06-benchmark"))

timings <-
  raw_data %>%
  filter(method_id == "identity") %>%
  select(method_id, dataset_id, dataset_trajectory_type, starts_with("time_"))

ggplot(timings %>% gather(step, time, starts_with("time_"))) +
  geom_density(aes(time, colour = step)) +
  facet_wrap(~ step, scales = "free") +
  labs(title = "Identity")

