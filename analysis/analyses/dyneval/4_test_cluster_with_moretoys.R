library(dynalysis)
library(tidyverse)

derived_dir <- "analysis/data/derived_data/dyneval/2_test_cluster_with_moretoys/"

# # remove previous output
# unlink(derived_dir, recursive=TRUE)

dir.create(derived_dir, recursive = T)

# easy test
tasks_file <- paste0(derived_dir, "tasks.RData")
if (file.exists(tasks_file)) {
  load(tasks_file)
} else {
  tasks <- generate_toy_datasets(num_replicates = 4)
  task_group <- rep("group", nrow(tasks))
  task_fold <- gsub(".*_", "", tasks$id) %>% as.integer()
  save(tasks, task_group, task_fold, file = tasks_file)
}
methods <- get_descriptions(as_tibble = T) %>% arrange(desc(name == "shuffle"))

# metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation",
#              "isomorphic", "ged", "robbie_network_score", "mantel_pval")
metrics <- "auc_R_nx"

benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_dir,
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = 120,
  memory = "8G",
  num_cores = 2,
  num_iterations = 200,
  num_init_params = 100
)

outputs <- benchmark_suite_retrieve(derived_dir)

failed <- outputs %>% filter(which_errored, error != "job is still running")

failed
failed %>% extract_row_to_list(1) %>% .$error %>% cat

succeeded <- outputs %>% filter(!which_errored) %>% group_by(method_name) %>% filter(n() == 4) %>% ungroup()

time_df <- bind_rows(succeeded$eval_ind) %>% select(method_name, starts_with("time_")) %>% group_by(method_name) %>% summarise_all(mean) %>%
  gather(part, time, -method_name) %>% mutate(part = gsub("time_", "", part))

method_name_u <- time_df$method_name %>% unique %>% sort(decreasing = T)
pdf(paste0(derived_dir, "/results_timings.pdf"), 10, 10)
ggplot(time_df %>% mutate(method_name = factor(method_name, levels = method_name_u))) +
  geom_bar(aes(method_name, time, fill = method_name), stat = "identity") +
  facet_wrap(~part, scales = "free") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Timings on toy datasets")
dev.off()

eval_grp <- bind_rows(succeeded$eval_grp) #%>% mutate(harmonicmean = 2 * correlation * robbie_network_score / (correlation + robbie_network_score))
param_sels <- eval_grp %>%
  filter(fold_type == "train") %>%
  group_by(ti_type, task_group, method_name, fold_i) %>%
  arrange(desc(auc_R_nx)) %>%
  slice(1) %>%
  ungroup() %>%
  select(ti_type, task_group, method_name, fold_i, param_i)
train_eval_grp <- eval_grp %>% filter(fold_type == "train") %>% inner_join(param_sels, by = colnames(param_sels))
test_eval_grp <- eval_grp %>% filter(fold_type == "test") %>% inner_join(param_sels, by = colnames(param_sels))

train_eval_grp_summ <- train_eval_grp %>% group_by(ti_type, task_group, method_name) %>% summarise_if(is.numeric, mean) %>% ungroup
test_eval_grp_summ <- test_eval_grp %>% group_by(ti_type, task_group, method_name) %>% summarise_if(is.numeric, mean) %>% ungroup

ordered_names <- test_eval_grp_summ %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% ungroup() %>% arrange(auc_R_nx) %>% .$method_name
train_eval_grp_summ <- train_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))
test_eval_grp_summ <- test_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))

g <- cowplot::plot_grid(
  ggplot(test_eval_grp_summ) +
    geom_bar(aes(method_name_f, auc_R_nx, fill = method_name_f), stat = "identity") +
    facet_wrap(~ti_type, ncol = 1) +
    coord_flip() +
    theme(legend.position = "none"),
  # ggplot(test_eval_grp_summ) +
  #   geom_bar(aes(method_name_f, correlation, fill = method_name_f), stat = "identity") +
  #   facet_wrap(~ti_type, ncol = 1) +
  #   coord_flip() +
  #   theme(legend.position = "none"),
  # ggplot(test_eval_grp_summ) +
  #   geom_bar(aes(method_name_f, robbie_network_score, fill = method_name_f), stat = "identity") +
  #   facet_wrap(~ti_type, ncol = 1) +
  #   coord_flip() +
  #   theme(legend.position = "none"),
  # ggplot(test_eval_grp_summ) +
  #   geom_bar(aes(method_name_f, harmonicmean, fill = method_name_f), stat = "identity") +
  #   facet_wrap(~ti_type, ncol = 1) +
  #   coord_flip() +
  #   theme(legend.position = "none"),
  nrow = 1
)

pdf(paste0(derived_dir, "/results_metrics_test.pdf"), 15, 15)
g
dev.off()
