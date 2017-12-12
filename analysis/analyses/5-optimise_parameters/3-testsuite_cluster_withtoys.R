library(dynalysis)
library(tidyverse)

experiment("5-optimize_parameters/3_testsuite_cluster_withtoys")

# # remove previous output
# unlink(derived_file("suite/"), recursive = T, force = T)

# trying all methods
methods <- dynmethods::get_descriptions() #%>% filter(name == "shuffle")

# toys
tasks <- dyntoy::toy_tasks[c(2,5),]
task_group <- rep("group", nrow(tasks))
task_fold <- gsub(".*_([0-9]*)$", "\\1", tasks$id) %>% as.integer

#metrics <- c("auc_R_nx", "correlation")
metrics <- "auc_R_nx"
timeout <- 600

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
  num_iterations = 5,
  num_repeats = 1,
  num_init_params = 16,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL
)

# outputs <- benchmark_suite_retrieve(derived_file("suite/"))

# save(outputs, file = result_file("outputs.RData"))
outputs <- load(file = result_file("outputs.RData"))

# outputs %>% rowwise() %>% mutate(memory = qacct$maxvmem) %>% ungroup() %>% View
# outputs$qsub_error[[1]][[1]] %>% cat



# out_sel <- outputs %>% filter(method_name == "shuffle")
# out_sel$qsub_error
# out_sel$individual_scores[[1]]$error[[1]]


succeeded <- outputs %>% filter(!sapply(which_errored, function(x) any(unlist(x)))) %>% group_by(method_name) %>% filter(n() == 2) %>% ungroup()

score_df <- bind_rows(succeeded$individual_scores) %>%
  mutate(pct_errored = 1-sapply(error, is.null)) %>%
  group_by(method_name) %>%
  summarise_if(is.numeric, mean) %>%
  select(-fold_i, -grid_i, -iteration_i, -param_i, -repeat_i) %>%
  gather(metric, score, -method_name)

method_name_u <- score_df$method_name %>% unique %>% sort(decreasing = T)
# pdf(figure_file("/results_metrics.pdf"), 10, 10)
ggplot(score_df %>% mutate(method_name = factor(method_name, levels = method_name_u))) +
  geom_bar(aes(method_name, score, fill = method_name), stat = "identity") +
  facet_wrap(~metric, scales = "free") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Timings on toy datasets")
# dev.off()


# # summary %>%
# #   select(method_name, correlation, mean_R_nx, auc_R_nx, Q_global, Q_local, num_files_created, num_setseed_calls) %>%
# #   arrange(auc_R_nx) %>%
# #   mutate(method_name_f = factor(method_name, levels = method_name)) %>%
# #   gather(metric, score, -method_name, -method_name_f)
#
# eval_grp <- bind_rows(succeeded$eval_ind) #%>% mutate(harmonicmean = 2 * correlation * robbie_network_score / (correlation + robbie_network_score))
# param_sels <- eval_grp %>%
#   filter(fold_type == "train") %>%
#   group_by(method_name, fold_i) %>%
#   arrange(desc(auc_R_nx)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(method_name, fold_i, param_i)
# train_eval_grp <- eval_grp %>% filter(fold_type == "train") %>% inner_join(param_sels, by = colnames(param_sels))
# test_eval_grp <- eval_grp %>% filter(fold_type == "test") %>% inner_join(param_sels, by = colnames(param_sels))
#
# train_eval_grp_summ <- train_eval_grp %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% ungroup
# test_eval_grp_summ <- test_eval_grp %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% ungroup
#
# ordered_names <- test_eval_grp_summ %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% ungroup() %>% arrange(auc_R_nx) %>% .$method_name
# train_eval_grp_summ <- train_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))
# test_eval_grp_summ <- test_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))
#
# g <- cowplot::plot_grid(
#   ggplot(test_eval_grp_summ) +
#     geom_bar(aes(method_name_f, auc_R_nx, fill = method_name_f), stat = "identity") +
#     coord_flip() +
#     theme(legend.position = "none"),
#   # ggplot(test_eval_grp_summ) +
#   #   geom_bar(aes(method_name_f, correlation, fill = method_name_f), stat = "identity") +
#   #   facet_wrap(~trajectory_type, ncol = 1) +
#   #   coord_flip() +
#   #   theme(legend.position = "none"),
#   # ggplot(test_eval_grp_summ) +
#   #   geom_bar(aes(method_name_f, robbie_network_score, fill = method_name_f), stat = "identity") +
#   #   facet_wrap(~trajectory_type, ncol = 1) +
#   #   coord_flip() +
#   #   theme(legend.position = "none"),
#   # ggplot(test_eval_grp_summ) +
#   #   geom_bar(aes(method_name_f, harmonicmean, fill = method_name_f), stat = "identity") +
#   #   facet_wrap(~trajectory_type, ncol = 1) +
#   #   coord_flip() +
#   #   theme(legend.position = "none"),
#   nrow = 1
# )
#
# pdf(figure_file("/results_metrics_test.pdf"), 15, 15)
# g
# dev.off()
