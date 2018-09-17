library(tidyverse)
library(extrafont)

dynbenchmark:::install_fontawesome()

extrafont::loadfonts()

method_statuses <- tribble(
  ~method_status, ~colour, ~fa, ~icon_colour,
  "execution_error", "#9d0549", "\uf00d", "white",
  "method_error", "#d51d14", "\uf057", "white",
  "memory_limit", "#f46b21", "\uf538", "black",
  "time_limit", "#f1be21", "\uf017", "black",
  "missing_prior", "#fff600", "\uf0cb", "black",
  "low_correlation", "#b4ef43", "\uf537", "black",
  "no_error", "#2ECC40", "\uf058", "black"
) %>%
  mutate(method_status = fct_inorder(method_status))

method_status_colours <-
  method_statuses %>% deframe()

devtools::use_data(method_statuses, method_status_colours, pkg = "package", overwrite = TRUE)

ggplot(method_statuses, aes(y = fct_rev(method_status))) +
  geom_tile(aes(x = 1, fill = method_status), size = 1, height = .9, width = .9) +
  geom_text(aes(x = 1, label = fa, colour = method_status), family = "Font Awesome 5 Free", size = 8) +
  geom_text(aes(x = 0, label = method_status), hjust = 1, size = 8) +
  scale_fill_manual(values = method_status_colours) +
  scale_colour_manual(values = method_statuses %>% select(method_status, icon_colour) %>% deframe()) +
  expand_limits(x = -2) +
  coord_equal() +
  cowplot::theme_nothing()
