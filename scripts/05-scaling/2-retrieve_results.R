library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results()

# bind results in one data frame (without models)
outputs <- benchmark_bind_results(load_models = TRUE)
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

timings <- joined %>% group_by(method_id) %>% do({ df <- filter(., error_status == "no_error"); if (nrow(df) > 0) df else slice(., 1) }) %>% ungroup()

g2 <- ggplot(timings, aes(lnrow, time_method, group = lncol, colour = lncol, alpha = error_status == "no_error")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  scale_y_log10() +
  scale_colour_distiller(palette = "RdYlBu") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  theme_classic() +
  theme(legend.position = "bottom")+
  facet_wrap(~method_id, ncol = 1, scales = "free_y") +
  labs(alpha = NULL)

g3 <- ggplot(timings, aes(lncol, time_method, group = lnrow, colour = lnrow, alpha = error_status == "no_error")) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
  scale_y_log10() +
  scale_colour_distiller(palette = "RdYlBu") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  theme_classic() +
  theme(legend.position = "bottom")+
  facet_wrap(~method_id, ncol = 1, scales = "free_y") +
  labs(alpha = NULL)

patchwork::wrap_plots(g1, g2, g3, nrow = 1)

intercepts <- joined %>%
  filter(error_status == "no_error", time_method > 3) %>%
  group_by(method_id) %>%
  do({
    dat <- .
    fit <- lm(log10(time_method) ~ lnrow + lncol, dat)
    t(fit$coefficients) %>% as.data.frame() %>% mutate(method_id = dat$method_id[[1]])
  }) %>%
  ungroup()

ggplot(intercepts, aes(lnrow, lncol)) +
  geom_point() +
  geom_text(aes(label = method_id), nudge_y = .03) +
  theme_classic()
