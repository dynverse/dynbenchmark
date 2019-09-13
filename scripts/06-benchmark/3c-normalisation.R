#' Compare the effect of normalisation on the results

stat_funs <- c("sd", "mean")
metricso <- c("overall", metrics)

preproc_fun <- function(x) {
  x[x < 0] <- 0
  x[x > 1] <- 1

  xnona <- x[!is.na(x)]
  xnazero <- ifelse(is.na(x), 0, x)

  if (length(xnazero) == 1 || all(xnazero == 0)) {
    x
  } else {
    dynbenchmark:::.benchmark_aggregate_normalisation$scalesigmoid(xnona, xnazero)$y
  }
}

dat_df <-
  data %>%
  select(method_id, dataset_id, !!metricso) %>%
  gather(metric, score, !!metricso) %>%
  group_by(dataset_id, metric) %>%
  filter(n() > 2) %>%
  rename(unnorm = score) %>%
  mutate(norm = preproc_fun(unnorm)) %>%
  gather(type, score, unnorm, norm) %>%
  mutate(type = factor(type, levels = c("unnorm", "norm"))) %>%
  ungroup()

var_df <-
  dat_df %>%
  group_by(type, dataset_id, metric) %>%
  summarise_at(vars(score), stat_funs) %>%
  ungroup()

g <- ggplot(var_df) +
  geom_point(aes(mean, sd, colour = metric)) +
  facet_wrap(~type) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()

# g
ggsave(result_file("normalisation_var_mean.pdf"), g, width = 10, height = 5)

rm(stat_funs, metricso, dat_df, var_df, g)

# direct compare normalised vs non-normalised
dircom <-
  data_aggregations %>%
  select(method_id:dataset_source, !!c(metrics, paste0("norm_", metrics))) %>%
  gather(metric, value, !!c(metrics, paste0("norm_", metrics))) %>%
  mutate(
    met = gsub("norm_", "", metric),
    ric = ifelse(grepl("norm_", metric), "norm", "unnorm")
  )
directcomp <-
  dircom %>%
  select(-metric) %>%
  spread(ric, value)

g1 <- ggplot(directcomp %>% filter(dataset_source == "mean")) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(unnorm, norm, colour = dataset_trajectory_type)) +
  theme_bw() +
  facet_grid(met~dataset_trajectory_type) +
  scale_colour_manual(values = trajtypes %>% select(id, colour) %>% deframe()) +
  labs(title = paste0("Unnormalised versus normalised, for dataset_source == 'mean'"))
ggsave(result_file("normalisation_unnorm-v-norm_trajtypes.pdf"), g1, width = 15, height = 8)

g2 <- ggplot(directcomp %>% filter(dataset_trajectory_type == "overall")) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(unnorm, norm, colour = dataset_source)) +
  theme_bw() +
  facet_grid(met~dataset_source) +
  labs(title = paste0("Unnormalised versus normalised, for dataset_source == 'mean'"))
ggsave(result_file("normalisation_unnorm-v-norm_sources.pdf"), g2, width = 11, height = 8)

rm(directcomp, dircom, g1, g2)

 # compare per dataset
dircom <- data %>%
  select(method_id, dataset_id, param_id, prior_id, repeat_ix, dataset_source, !!c(metrics, paste0("norm_", metrics))) %>%
  gather(metric, value, !!c(metrics, paste0("norm_", metrics))) %>%
  mutate(
    met = gsub("norm_", "", metric),
    ric = ifelse(grepl("norm_", metric), "norm", "unnorm")
  )
dircom_spr <-
  dircom %>%
  select(-metric) %>%
  spread(ric, value)

dids <- unique(dircom_spr$dataset_id)
# ix <- sample.int(length(dids), 20) %>% sort()
# dids <- dids[ix]

pdf(result_file("normalisation_unnorm-v-norm_dataset.pdf"), width = 15, height = 3)
for (did in dids) {
  cat(did, "\n", sep = "")
  g <- ggplot(dircom_spr %>% filter(dataset_id == did)) +
    geom_point(aes(unnorm, norm, colour = met)) +
    facet_grid(dataset_id ~ met) +
    theme_bw() +
    scale_colour_brewer(palette = "Dark2") +
    labs(title = did)
  print(g)
}
dev.off()

rm(dircom, dircom_spr, dids, g)
