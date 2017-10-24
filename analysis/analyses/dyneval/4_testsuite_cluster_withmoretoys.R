library(dynalysis)
library(tidyverse)

experiment(
  dirname = "dyneval/4_testsuite_cluster_withmoretoys",
  description = "Testing whether each method is able to run on the cluster",
  auto_create_folders = TRUE
)

# # remove previous output
# unlink(scratch_file("suite/"), recursive = T, force = T)

# trying all methods
methods <- dyneval::get_descriptions() #%>% filter(name == "shuffle")

# toys
tasks <- dyntoy::toy_tasks
task_group <- rep("group", nrow(tasks))
task_fold <- gsub(".*_([0-9]*)$", "\\1", tasks$id) %>% as.integer

#metrics <- c("auc_R_nx", "correlation")
metrics <- "auc_R_nx"
timeout <- 60

# start benchmark suite
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = scratch_file("suite/"),
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

outputs <- benchmark_suite_retrieve(scratch_file("suite/"))

outputs2 <- outputs %>% rowwise() %>% mutate(any_errored = any(unlist(which_errored)), memory = qacct$maxvmem) %>% ungroup()


View(outputs2 %>% select(-qsub_error:-eval_grp))



# select only the runs that succeeded
succeeded <- outputs2 %>% filter(!any_errored) %>% group_by(method_name) %>% filter(n() == 6) %>% ungroup()

# bind the metrics of the individual runs
eval_ind <-
  bind_rows(succeeded$eval_ind)

# summarising at a global level
summ <- eval_ind %>%
  group_by(method_name, fold_type, grid_i, repeat_i, fold_i, group_sel, param_i, iteration_i) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# finding the best parameters on the train
best_parm <- summ %>%
  filter(fold_type == "train") %>%
  group_by(method_name, grid_i, repeat_i, fold_i, group_sel) %>%
  arrange(desc(auc_R_nx)) %>%
  slice(1) %>%
  ungroup() %>%
  select(method_name, grid_i, repeat_i, fold_i, group_sel, param_i)

# filtering the summary for the best parms
best_summ <- summ %>%
  right_join(best_parm, by = colnames(best_parm))

# aggregating the scores of the best configuration for each fold
best_summ_agg <- best_summ %>%
  group_by(method_name, fold_type, repeat_i, group_sel) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  select(-grid_i, -fold_i, -param_i, -iteration_i)

# gathering the different metrics
best_summ_agg_spr <- best_summ_agg %>% gather(metric, value, -method_name:-group_sel)

# plot
ggplot(best_summ_agg_spr) +
  geom_bar(aes(method_name, value, fill = fold_type), stat = "identity", position = position_dodge()) +
  facet_wrap(~metric, scales = "free") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(title = )
