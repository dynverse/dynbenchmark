#' Compare the predicted execution times and memory usages with the actual ones

library(tidyverse)
library(dynbenchmark)

experiment("10-benchmark_interpretation")

methods <- load_methods()

models <- read_rds(result_file("scaling.rds", "05-scaling"))$models
time_models <- models %>% select(method_id, predict_time) %>% deframe()
mem_models <- models %>% select(method_id, predict_mem) %>% deframe()

datasets <- mapdf_dfr(load_datasets(), function(ds) {
  expr <- get_expression(ds)
  data_frame(dataset_id = ds$id, nrow = nrow(expr), ncol = ncol(expr))
})
benchmark <- read_rds(result_file("benchmark_results_unnormalised.rds", "06-benchmark")) %>%
  filter(method_id %in% methods$id) %>%
  transmute(experiment = "benchmark", method_id, dataset_id, error_status, time, mem) %>%
  left_join(datasets, by = "dataset_id")
scaling <- read_rds(result_file("scaling.rds", "05-scaling"))$data %>%
  filter(method_id %in% methods$id) %>%
  transmute(experiment = "scaling", method_id, dataset_id, error_status, nrow, ncol, time = time_method, mem = max_mem)

timings <- bind_rows(benchmark, scaling) %>%
  filter(error_status == "no_error") %>%
  group_by(method_id) %>%
  mutate(
    time_pred = time_models[[method_id[[1]]]](nrow, ncol),
    mem_pred = mem_models[[method_id[[1]]]](nrow, ncol)
  ) %>%
  ungroup() %>%
  mutate(label = ifelse(experiment == "benchmark", "Test (benchmark datasets)", "Training (scaling datasets)") %>% factor(levels = c("Training (scaling datasets)", "Test (benchmark datasets)")))

cors <-
  timings %>%
  group_by(experiment, label) %>%
  summarise(
    cor_time = nacor(log10(time), log10(time_pred)),
    cor_mem = nacor(log10(mem), log10(mem_pred))
  ) %>%
  ungroup()

g1 <- ggplot() +
  geom_abline(aes(intercept = intercept, slope = slope), data_frame(intercept = 0, slope = 1)) +
  geom_point(aes(log10(time_pred), log10(time), colour = experiment), timings, size = .1, alpha = .5) +
  geom_text(aes(-1, 3, label = paste0("Cor = ", round(cor_time, 2))), cors) +
  theme_bw() +
  facet_wrap(~ label) +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Predicted log10 execution time", y = "Actual log10 execution time")

g2 <- ggplot() +
  geom_abline(aes(intercept = intercept, slope = slope), data_frame(intercept = 0, slope = 1)) +
  geom_point(aes(log10(mem_pred), log10(mem), colour = experiment), timings %>% filter(log10(mem) > 7.5), size = .1, alpha = .5) +
  geom_text(aes(8.5, 10, label = paste0("Cor = ", round(cor_mem, 2))), cors) +
  theme_bw() +
  facet_wrap(~ label) +
  theme(legend.position = "none") +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Predicted log10 memory usage", y = "Actual log10 memory usage")

g <- patchwork::wrap_plots(g1, g2, ncol = 1)

ggsave(result_file("predcors.pdf"), g, width = 8, height = 8)

