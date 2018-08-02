library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results()

# bind results in one data frame (without models)
outputs <- benchmark_bind_results(load_models = FALSE) %>%
  mutate(error_status = ifelse(time_postprocessing < .1 & error_status == "no_error", "execution_error", error_status))
design <- read_rds(derived_file("design.rds"))
datasets <- design$datasets

ggplot(outputs) + geom_point(aes(time_method, time_postprocessing, colour = error_status)) + scale_x_log10() + scale_y_log10() + scale_colour_brewer(palette = "Dark2") + theme_classic()

axis_scale <- datasets %>% select(lnrow, nrow) %>% unique() %>% filter(lnrow %% 1 == 0)

joined <-
  outputs %>%
  select(method_id, dataset_id, errored = dummy, error_status, starts_with("time_")) %>%
  left_join(datasets %>% select(dataset_id = id, lnrow, lncol, lsum, nrow, ncol, memory), by = "dataset_id")

g1 <-
  ggplot(joined) +
  geom_tile(aes(lnrow, lncol, fill = error_status)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_wrap(~method_id, ncol = 1)

g2 <- ggplot(joined %>% filter(error_status == "no_error"), aes(lnrow, time_method, group = lncol, colour = lncol)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  scale_y_log10() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme_classic() +
  theme(legend.position = "bottom")+
  facet_wrap(~method_id, ncol = 1)

g3 <- ggplot(joined %>% filter(error_status == "no_error"), aes(lncol, time_method, group = lnrow, colour = lnrow)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  scale_y_log10() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme_classic() +
  theme(legend.position = "bottom")+
  facet_wrap(~method_id, ncol = 1)

patchwork::wrap_plots(g1, g2, g3, nrow = 1)
