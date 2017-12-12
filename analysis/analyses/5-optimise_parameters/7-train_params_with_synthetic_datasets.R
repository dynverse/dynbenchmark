library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimize_parameters/7-train_params_with_synthetic_datasets")

# tasks
tasks <- readRDS(derived_file("v5.rds", experiment_id = "datasets/synthetic"))
tasks_info <- tasks$info %>% map_df(as_data_frame) %>% mutate(task_id = tasks$id)
# tasks_info <- tasks_info %>% group_by(modulenet_name) %>% mutate(fold = sample.int(n())) %>% ungroup()

# save(tasks_info, file = derived_file("tasks_info.RData"))
load(derived_file("tasks_info.RData"))

# configure run
task_group <- rep("group", nrow(tasks))
task_fold <- tasks_info$fold
method_order <- c("random", "shuffle", "identity", "slgnsht", "SCORPIUS", "embeddr", "slngsht", "waterfll", "DPT", "mfa", "phenopat", "mnclDDR", "mnclICA", "Wishbone", "CTmaptpx", "CTVEM", "CTGibbs", "SLICER")
methods <- get_descriptions(as_tibble = T)
all_method_orders <- c(method_order, setdiff(methods$short_name, method_order))

# save(all_method_orders, file = derived_file("all_method_orders.RData"))
load(derived_file("all_method_orders.RData"))

methods <- methods %>% slice(match(all_method_orders, short_name))

#metrics <- c("auc_R_nx", "correlation")
metrics <- "auc_R_nx"
timeout <- 300

# start benchmark suite
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_file("suite/"),
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = timeout,
  memory = "11G",
  num_cores = 1,
  num_iterations = 100,
  num_repeats = 4,
  num_init_params = 200,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL
)


outputs <- benchmark_suite_retrieve(derived_file("suite/"))

# save(outputs, file = derived_file("outputs.RData"))
# outputs <- load(file = derived_file("outputs.RData"))

outputs2 <- outputs %>%
  rowwise() %>%
  mutate(
    any_errored = any(unlist(which_errored)),
    memory = ifelse(!is.null(qacct), qacct$maxvmem, NA)
  ) %>%
  ungroup()

num_repeats <- 4
num_folds <- 2
num_iterations <- 100
num_cores <- 1
num_init_params <- 200

# select only the runs that succeeded
succeeded <- outputs2 %>%
  filter(!any_errored) %>%
  group_by(method_name) %>%
  filter(n() == num_folds * num_repeats) %>%
  ungroup()

# bind the metrics of the individual runs

new_param_i_fun <- function(iteration_i, param_i) {
  ifelse(
    param_i == 1,
    0,
    ifelse(
      iteration_i == 0,
      sample.int(num_init_params)[seq_along(param_i)],
      sample.int(num_cores)[seq_along(param_i)] + ((iteration_i-1) * num_cores) + num_init_params
    )
  )
}

eval_ind <-
  bind_rows(succeeded$individual_scores) %>%
  left_join(tasks_info %>% select(task_id, trajectory_type = modulenet_name, platform_name), by = "task_id") %>%
  mutate(pct_errored = 1 - sapply(error, is.null))

priors <- eval_ind %>%
  group_by(method_name) %>%
  slice(1) %>%
  rowwise() %>%
  mutate(prior = ifelse(nrow(prior_df) == 0, "", paste(prior_df$prior_names, "--", prior_df$prior_type, sep = "", collapse = ";"))) %>%
  ungroup() %>%
  select(method_name, prior, prior_df)


param_i_map <- eval_ind %>%
  mutate(is_default = param_i == 1) %>%
  group_by(grid_i, fold_i, iteration_i, param_i, is_default) %>%
  summarise() %>%
  mutate(newparam_i = new_param_i_fun(iteration_i, param_i)) %>%
  ungroup()

eval_ind <- eval_ind %>% left_join(param_i_map, by = c("grid_i", "fold_i", "iteration_i", "param_i"))

# ggplot(eval_ind) + geom_point(aes(newparam_i, iteration_i, colour = method_name))

# summarising at a global level
summ <- eval_ind %>%
  group_by(method_name, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, newparam_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# finding the best parameters on the train
best_parm <-
  bind_rows(succeeded$best) %>%
  mutate(method_name = succeeded$method_name) %>%
  select(method_name, group_sel, fold_i, repeat_i, grid_i, param_index,
         params, y_names, train_score, test_score, .object_class)

best_parm2 <- summ %>%
  filter(fold_type == "train") %>%
  group_by(method_name, grid_i, repeat_i, fold_i, group_sel) %>%
  arrange(desc(auc_R_nx)) %>%
  slice(1) %>%
  ungroup() %>%
  select(method_name, grid_i, repeat_i, fold_i, group_sel, param_i, newparam_i)

# filtering the summary for the best parms
best_summ <- summ %>%
  right_join(best_parm2, by = colnames(best_parm2))
default_summ <- summ %>% filter(param_i == 1)

# aggregating the scores of the best configuration for each fold
best_summ_agg <- best_summ %>%
  group_by(method_name, fold_type, repeat_i, group_sel) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -newparam_i, -iteration_i)
default_summ_agg <- default_summ %>%
  group_by(method_name, fold_type, repeat_i, group_sel) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -newparam_i, -iteration_i)

# gathering the different metrics
best_summ_agg_spr <- best_summ_agg %>% gather(metric, value, -method_name:-group_sel)
default_summ_agg_spr <- default_summ_agg %>% gather(metric, value, -method_name:-group_sel)

meth_ord <- best_summ_agg %>% filter(fold_type == "test") %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% arrange(desc(auc_R_nx)) %>% .$method_name



####################
## START PLOTTING ##
###################3
best_sel <- best_summ_agg_spr %>% group_by(method_name, fold_type, group_sel, metric) %>% summarise(value = mean(value)) %>% ungroup()
default_sel <- default_summ_agg_spr %>% group_by(method_name, fold_type, group_sel, metric) %>% summarise(value = mean(value)) %>% ungroup()

pdf(figure_file("all_scores.pdf"), 20, 15)
ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = metric)) +
  geom_bar(stat = "identity", data = best_sel %>% filter(fold_type == "test")) +
  geom_point(aes(shape = "train"), data = best_sel %>% filter(fold_type == "train")) +
  geom_point(aes(shape = "default param"), data = default_sel %>% filter(fold_type == "test")) +
  facet_wrap(~metric, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "score",
    title = pritt(
      "Dyneval parameter optimisation on in silico datasets\n",
      "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
      "point = train score, bar = test score, cirle = score of default params on test data."
    )
  )
dev.off()

pdf(figure_file("all_scores_per_repeat.pdf"), 20, 15)
for (repi in seq_len(num_repeats)) {
  best_sel <- best_summ_agg_spr %>% filter(repeat_i == repi)
  default_sel <- default_summ_agg_spr %>% filter(repeat_i == repi)
  g <- ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = metric)) +
    geom_bar(stat = "identity", data = best_sel %>% filter(fold_type == "test")) +
    geom_point(aes(shape = "train"), data = best_sel %>% filter(fold_type == "train")) +
    geom_point(aes(shape = "default param"), data = default_sel %>% filter(fold_type == "test")) +
    facet_wrap(~metric, scales = "free") +
    cowplot::theme_cowplot() +
    coord_flip() +
    labs(
      x = NULL,
      y = "score",
      title = pritt(
        "Dyneval parameter optimisation on in silico datasets; REPEAT {repi} / {num_repeats}\n",
        "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
        "point = train score, bar = test score, cirle = score of default params on test data."
      )
    )
  print(g)
}
dev.off()


# by group
best_grp <- eval_ind %>%
  group_by(method_name, trajectory_type, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, newparam_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  right_join(best_parm2, by = colnames(best_parm2)) %>%
  group_by(method_name, fold_type, repeat_i, group_sel, trajectory_type) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -newparam_i, -iteration_i) %>%
  gather(metric, value, -method_name:-group_sel, -trajectory_type)
best_grp <- bind_rows(best_grp, best_summ_agg_spr %>% mutate(trajectory_type = "overall"))
default_grp <- eval_ind %>%
  group_by(method_name, trajectory_type, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, newparam_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  filter(param_i == 1) %>%
  group_by(method_name, fold_type, repeat_i, group_sel, trajectory_type) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -newparam_i, -iteration_i) %>%
  gather(metric, value, -method_name:-group_sel, -trajectory_type)
default_grp <- bind_rows(default_grp, default_summ_agg_spr %>% mutate(trajectory_type = "overall"))


best_grp_sel <- best_grp %>% group_by(method_name, fold_type, group_sel, metric, trajectory_type) %>% summarise(value = mean(value)) %>% ungroup()
default_grp_sel <- default_grp %>% group_by(method_name, fold_type, group_sel, metric, trajectory_type) %>% summarise(value = mean(value)) %>% ungroup()

pdf(figure_file("by-ti-type_auc-R-nx.pdf"), 20, 15)
ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = trajectory_type)) +
  geom_bar(stat = "identity", data = best_grp_sel %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  geom_point(aes(shape = "train"), data = best_grp_sel %>% filter(fold_type == "train", metric == "auc_R_nx")) +
  geom_point(aes(shape = "default param"), data = default_grp_sel %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  facet_wrap(~trajectory_type, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "auc_R_nx",
    title = pritt(
      "Dyneval parameter optimisation on in silico datasets\n",
      "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
      "point = train score, bar = test score, cirle = score of default params on test data."
    )
  )
dev.off()

pdf(figure_file("by-ti-type_auc-R-nx_per_repeat.pdf"), 20, 15)
for (repi in seq_len(num_repeats)) {
  best_grp_sel <- best_grp %>% filter(repeat_i == repi)
  default_grp_sel <- default_grp %>% filter(repeat_i == repi)

  g <- ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = trajectory_type)) +
    geom_bar(stat = "identity", data = best_grp_sel %>% filter(fold_type == "test", metric == "auc_R_nx")) +
    geom_point(aes(shape = "train"), data = best_grp_sel %>% filter(fold_type == "train", metric == "auc_R_nx")) +
    geom_point(aes(shape = "default param"), data = default_grp_sel %>% filter(fold_type == "test", metric == "auc_R_nx")) +
    facet_wrap(~trajectory_type, scales = "free") +
    cowplot::theme_cowplot() +
    coord_flip() +
    labs(
      x = NULL,
      y = "auc_R_nx",
      title = pritt(
        "Dyneval parameter optimisation on in silico datasets\n",
        "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
        "point = train score, bar = test score, cirle = score of default params on test data."
      )
    )
  print(g)
}
dev.off()
pdf(figure_file("by-ti-type_auc-R-nx_per_repeat2.pdf"), 40, 15)
ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = trajectory_type)) +
  geom_bar(stat = "identity", data = best_grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  geom_point(aes(shape = "train"), data = best_grp %>% filter(fold_type == "train", metric == "auc_R_nx")) +
  geom_point(aes(shape = "default param"), data = default_grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  facet_grid(repeat_i~trajectory_type, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "auc_R_nx",
    title = pritt(
      "Dyneval parameter optimisation on in silico datasets\n",
      "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
      "point = train score, bar = test score, cirle = score of default params on test data."
    )
  )
dev.off()

pdf(figure_file("by-ti-type_auc-R-nx_per_repeat3.pdf"), 30, 15)
ggplot(mapping = aes(repeat_i, value, fill = trajectory_type)) +
  geom_bar(stat = "identity", data = best_grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  geom_point(aes(shape = "train"), data = best_grp %>% filter(fold_type == "train", metric == "auc_R_nx")) +
  geom_point(aes(shape = "default param"), data = default_grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  facet_grid(method_name~trajectory_type, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "auc_R_nx",
    title = pritt(
      "Dyneval parameter optimisation on in silico datasets\n",
      "({num_init_params} initial, {num_iterations} iterations of {num_cores} new parameters, {num_repeats} repeats)\n",
      "point = train score, bar = test score, cirle = score of default params on test data."
    )
  )
dev.off()

# see performance over iterations
compare <- summ %>%
  gather(metric, score, -method_name:-iteration_i) %>%
  mutate(combine = paste0(fold_type, ".", metric)) %>%
  select(-fold_type, -metric) %>%
  spread(combine, score)

method_names <- unique(compare$method_name)
pdf(figure_file("paramoptim-auc_R_nx.pdf"), 16, 8)
for (mn in method_names) {
  g <- ggplot(compare %>% filter(method_name == mn)) +
    geom_point(aes(train.auc_R_nx, test.auc_R_nx, colour = iteration_i)) +
    scale_colour_distiller(palette = "RdBu") +
    facet_grid(fold_i~repeat_i) +
    theme_bw() +
    labs(title = pritt(
      "Comparing train and test auc_R_nx scores over training iterations\n",
      "Method {mn} -- repeats versus folds"
    ))
  print(g)
}
dev.off()

pdf(figure_file("paramoptim-auc_R_nx-xparami.pdf"), 16, 8)
for (mn in method_names) {
  g <- ggplot(summ %>% filter(method_name == mn), aes(newparam_i, auc_R_nx, fill = fold_type, colour = fold_type)) +
    geom_smooth(aes(colour = NA), span=.5) +
    geom_point() +
    geom_vline(xintercept = num_init_params) +
    facet_grid(fold_i~repeat_i) +
    cowplot::theme_cowplot() +
    labs(title = pritt(
      "Comparing train and test auc_R_nx scores over training iterations\n",
      "Method {mn} -- repeats versus folds"
    )) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Set2")
  print(g)
}
dev.off()


summ_best_periter <- summ %>% group_by(method_name, fold_type, grid_i, repeat_i, fold_i, group_sel, iteration_i) %>% arrange(desc(auc_R_nx)) %>% slice(1) %>% ungroup()
pdf(figure_file("paramoptim-auc_R_nx-xiteri.pdf"), 16, 8)
for (mn in method_names) {
  g <- ggplot(summ_best_periter %>% filter(method_name == mn), aes(newparam_i, auc_R_nx, fill = fold_type, colour = fold_type)) +
    geom_smooth(aes(colour = NA), span=.5) +
    geom_point() +
    facet_grid(fold_i~repeat_i) +
    cowplot::theme_cowplot() +
    labs(title = pritt(
      "Comparing train and test auc_R_nx scores over training iterations\n",
      "Method {mn} -- repeats versus folds"
    )) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Set2")
  print(g)
}
dev.off()




# check errored methods
errored <- outputs2 %>% filter(any_errored)
errored$method_name %>% unique

err_spec <- errored %>% filter(method_name == "Mpath")
err_spec$qsub_error[[1]]
err_spec$individual_scores[[1]]$error[[2]]
