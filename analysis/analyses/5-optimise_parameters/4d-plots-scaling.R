library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")

# extract dimensionality from tasks
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
tasks_info <- map_df(
  paste0(local_tasks_folder, "/", task_ids, ".rds"),
  function(task_id) {
    read_rds(task_id) %>%
      mutate(ncell = nrow(expression[[1]]), nfeat = ncol(expression[[1]])) %>%
      select(task_id = id, ncell, nfeat)
  }
)

# read timings
list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

# combine data and only keep what we need
oi <-
  outputs_ind %>%
  left_join(tasks_info, by = "task_id") %>%
  select(method_name, method_short_name, task_id, pct_errored, ncell, nfeat, time_method)

# select methods for which a timeout line should be drawn
timeout_lines <- oi %>%
  group_by(method_short_name) %>%
  summarise(max = max(time_method)) %>%
  filter(max > .5 * timeout_per_execution) %>%
  mutate(timeout_per_execution)

# fit linear models on timings
oi_datas <-
  oi %>%
  filter(pct_errored == 0) %>%
  group_by(method_name, method_short_name) %>%
  select(method_name, method_short_name, time_method, ncell, nfeat) %>%
  mutate(
    ncell2 = ncell^2,
    nfeat2 = nfeat^2,
    ncell_t_nfeat = ncell * nfeat,
    ncell2_t_nfeat = ncell2 * nfeat,
    ncell_t_nfeat2 = ncell * nfeat2,
    ncell2_t_nfeat2 = ncell2 * nfeat2,
    ncell_p_nfeat = ncell + nfeat,
    ncell2_p_nfeat = ncell2 + nfeat,
    ncell_p_nfeat2 = ncell + nfeat2,
    ncell2_p_nfeat2 = ncell2 + nfeat2
  ) %>%
  ungroup()

rsq_cutoff <- .25

oi_lm <- oi %>%
  filter(pct_errored == 0) %>%
  group_by(method_name, method_short_name) %>%
  do({
    dat <- .
    prefix <- dat %>%
      select(method_name, method_short_name) %>%
      slice(1)
    df <- oi_datas %>% inner_join(prefix %>% select(-method_name), by = "method_short_name") %>% select(-method_name, -method_short_name)
    feat_lvls <- colnames(df)[-1]

    lm_fits <- df %>%
      gather(feature, value, -time_method) %>%
      group_by(feature) %>%
      do({
        dat <- .
        fn <- unique(dat$feature)
        datdat <- dat %>% select(-feature)
        lm_fit <- lm(value ~ ., datdat)
        sum <- summary(lm_fit)
        data_frame(feature = fn, intercept = lm_fit$coefficients[[1]], slope = lm_fit$coefficients[[2]], rsq = sum$r.squared)
      }) %>%
      ungroup() %>%
      mutate(feature = factor(feature, levels = feat_lvls))

    prefix %>% crossing(lm_fits)
  }) %>%
  mutate(selected = rsq > rsq_cutoff & rsq == max(rsq)) %>%
  ungroup()

# compare rsq values
g <- ggplot(oi_lm) +
  geom_bar(aes(fct_rev(feature), rsq, fill = selected), stat = "identity") +
  cowplot::theme_cowplot() +
  coord_flip() +
  facet_wrap(~method_name) +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "darkgray"))

ggsave(figure_file("dimensionality_versus_timings_lmoverview.pdf"), g, width = 12, height = 14)

# determine selected complexities
complexity <- oi_lm %>%
  group_by(method_name, method_short_name) %>%
  arrange(desc(rsq)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    feature = ifelse(rsq < rsq_cutoff, "nothing", as.character(feature)),
    feature2 = sub("_._", "_", feature),
    sign = ifelse(grepl("_", feature), c("p"="+", "t"="×")[gsub("[^_]*_?([^_]*)_?.*", "\\1", feature)], "~")
  )



# generate direct comparison plots per method
mns <- unique(oi$method_short_name)

# pdf(figure_file("dimensionality_versus_timings_permethod.pdf"), 16, 12)
# for (m in mns) {
#   cat("Processing ", m, "\n", sep = "")
#   oim <- oi_datas %>% filter(method_short_name == m) %>% gather(feature, value, -method_name:-time_method)
#   lm_fits <- oi_lm %>% filter(method_short_name == m) %>% mutate(feature = as.character(feature))
#
#   slope <- oim %>%
#     left_join(lm_fits, by = "feature") %>%
#     mutate(y = time_method * slope + intercept)
#
#   g <- ggplot() +
#     geom_point(aes(time_method, value), oim ) +
#     geom_abline(aes(intercept = intercept, slope = slope), lm_fits, colour = "red", linetype = "dashed") +
#     theme_bw() +
#     facet_wrap(~feature, scales = "free_y", nrow = 3) +
#     scale_color_brewer(palette = "Dark2") +
#     labs(y = "Feature value", x = "Execution time (s)", title = m)
#   print(g)
# }
# dev.off()




# perform dimred
comp_mat <- oi_lm %>% select(method_short_name, feature, rsq) %>% reshape2::acast(method_short_name~feature, value.var = "rsq")
dist <- SCORPIUS::euclidean_distance(comp_mat)
space <- SCORPIUS::reduce_dimensionality(dist)
space_df <- space %>%
  as.data.frame() %>%
  rownames_to_column("method_short_name") %>%
  left_join(complexity, by = "method_short_name")

compcols <- c(
  nothing = "#d2d2d2",
  ncell = "#adb7f7",
  ncell2 = "#0f2191",
  nfeat = "#f7b4ad",
  ncell_nfeat = "#e0adf7",
  ncell2_nfeat = "#8a65ef",
  nfeat2 = "#911b0f",
  ncell_nfeat2 = "#ef6597",
  ncell2_nfeat2 = "#7b11a9"
)
g <- ggplot(space_df, aes(Comp1, Comp2, colour = factor(feature2, levels = names(compcols)))) +
  geom_point(aes(x, y), alpha = 0, data_frame(x = 0, y = 0, feature2 = names(compcols))) +
  geom_point(aes(shape = sign), size = 4) +
  geom_text(aes(label = method_short_name), nudge_y = .025) +
  cowplot::theme_cowplot() +
  scale_colour_manual(values = compcols) +
  scale_shape_manual(values = c("+" = 3, "×" = 4, "~" = 1)) +
  labs(colour = "Feature", shape = "plus or times") +
  guides(colour = guide_legend(ncol = 3)) +
  coord_equal()
g

ggsave(figure_file("dimensionality_versus_timings_lmdimred.pdf"), g, width = 10, height = 6)

# write derived data
write_rds(
  lst(oi, oi_lm, oi_datas, complexity),
  derived_file("dimensionality_versus_timings_data.rds")
)
