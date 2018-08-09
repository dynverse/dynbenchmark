library(tidyverse)

method_status_colours <- c(
  execution_error = "#9d0549",
  method_error = "#d51d14",
  memory_limit = "#f46b21",
  time_limit = "#f1be21",
  missing_prior = "#fff600",
  low_correlation = "#b4ef43",
  no_error = "#2ECC40"
)

tibble::enframe(method_status_colours) %>%
  mutate(i = seq_len(n())) %>%
  ggplot() +
  geom_text(aes(.4, i, label = name), hjust = 1) +
  geom_tile(aes(1, i, fill = name)) +
  scale_fill_manual(values = method_status_colours) +
  expand_limits(x = -0) +
  cowplot::theme_nothing()

devtools::use_data(method_status_colours, pkg = "package")
