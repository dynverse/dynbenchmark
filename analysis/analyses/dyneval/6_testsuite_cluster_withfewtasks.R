library(dynalysis)
library(tidyverse)
library(dynplot)

experiment(
  dirname = "dyneval/6_testsuite_cluster_withfewtasks",
  description = "Testing whether each method is able to get optimised on the cluster with a dyngen task",
  auto_create_folders = TRUE
)

# trying all methods
methods <- dyneval::get_descriptions(F)

# tasks
tasks <- readRDS("analysis/data/derived_data/dyngen/tasks_v4.rds") %>%
  filter(
    platform_id == "fluidigm_c1",
    takesetting_type == "snapshot",
    ti_type == "consecutive_bifurcating",
    model_replicate %in% c(1,2))

# configure run
task_group <- rep("group", nrow(tasks))
task_fold <- tasks$model_replicate
methods <- get_descriptions(as_tibble = T)

#metrics <- c("auc_R_nx", "correlation")
metrics <- "auc_R_nx"
timeout <- 120

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
  memory = "16G",
  num_cores = 4,
  num_iterations =  100,
  num_repeats = 1,
  num_init_params = 100
)

outputs <- benchmark_suite_retrieve(derived_file("suite/"))

save(outputs, file = result_file("outputs.RData"))

outputs2 <- outputs %>%
  rowwise() %>%
  mutate(
    any_errored = any(unlist(which_errored)),
    memory = ifelse(!is.null(qacct), qacct$maxvmem, NA)
  ) %>%
  ungroup()



# select only the runs that succeeded
succeeded <- outputs2 %>% filter(!any_errored) %>% group_by(method_name) %>% filter(n() == 2) %>% ungroup()

# bind the metrics of the individual runs
eval_ind <-
  bind_rows(succeeded$individual_scores) %>%
  left_join(tasks %>% select(task_id = id, ti_type), by = "task_id")

# summarising at a global level
summ <- eval_ind %>%
  group_by(method_name, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# finding the best parameters on the train
best_parm <-
  bind_rows(succeeded$best) %>%
  mutate(method_name = succeeded$method_name)

best_parm2 <- summ %>%
  filter(fold_type == "train") %>%
  group_by(method_name, grid_i, repeat_i, fold_i, group_sel) %>%
  arrange(desc(auc_R_nx)) %>%
  slice(1) %>%
  ungroup() %>%
  select(method_name, grid_i, repeat_i, fold_i, group_sel, param_i)

# filtering the summary for the best parms
best_summ <- summ %>%
  right_join(best_parm2, by = colnames(best_parm2))
default_summ <- summ %>% filter(param_i == 1)

# aggregating the scores of the best configuration for each fold
best_summ_agg <- best_summ %>%
  group_by(method_name, fold_type, repeat_i, group_sel) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -iteration_i)
default_summ_agg <- default_summ %>%
  group_by(method_name, fold_type, repeat_i, group_sel) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -iteration_i)

# gathering the different metrics
best_summ_agg_spr <- best_summ_agg %>% gather(metric, value, -method_name:-group_sel)
default_summ_agg_spr <- default_summ_agg %>% gather(metric, value, -method_name:-group_sel)

meth_ord <- best_summ_agg %>% filter(fold_type == "test") %>% arrange(desc(auc_R_nx)) %>% .$method_name

# plot
pdf(figure_file("all_scores.pdf"), 20, 15)
ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = metric)) +
  geom_bar(stat = "identity", data = best_summ_agg_spr %>% filter(fold_type == "test")) +
  # geom_point(aes(shape = "test"), data = best_summ_agg_spr %>% filter(fold_type == "test")) +
  geom_point(aes(shape = "train"), data = best_summ_agg_spr %>% filter(fold_type == "train")) +
  geom_point(aes(shape = "default param"), data = default_summ_agg_spr %>% filter(fold_type == "test")) +
  facet_wrap(~metric, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "score",
    title = paste0("Dyneval parameter optimisation on 2 in silico datasets\n100 initial, 100 iterations of 4 new parameters)\npoint = train score, bar = test score, cirle = score of default params on test data.")
  )
dev.off()


# by group
best_grp <- eval_ind %>%
  group_by(method_name, ti_type, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  right_join(best_parm2, by = colnames(best_parm2)) %>%
  group_by(method_name, fold_type, repeat_i, group_sel, ti_type) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -iteration_i) %>%
  gather(metric, value, -method_name:-group_sel, -ti_type)
default_grp <- eval_ind %>%
  group_by(method_name, ti_type, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  filter(param_i == 1) %>%
  group_by(method_name, fold_type, repeat_i, group_sel, ti_type) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -iteration_i) %>%
  gather(metric, value, -method_name:-group_sel, -ti_type)


pdf(figure_file("by-ti-type_auc-R-nx.pdf"), 20, 15)
ggplot(mapping = aes(factor(method_name, levels = rev(meth_ord)), value, fill = ti_type)) +
  geom_bar(stat = "identity", data = grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  geom_point(data = best_grp %>% filter(fold_type == "train", metric == "auc_R_nx")) +
  geom_point(data = default_grp %>% filter(fold_type == "test", metric == "auc_R_nx")) +
  facet_wrap(~ti_type, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(
    x = NULL,
    y = "auc_R_nx",
    title = paste0("Dyneval parameter optimisation on 2 in silico datasets\n100 initial, 100 iterations of 4 new parameters)\npoint = train score, bar = test score, cirle = score of default params on test data.")
  )
dev.off()
