library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

# settings
methods <- get_descriptions()
metrics <- c("correlation", "rf_mse", "edge_flip")
extra_metrics <- c()
eval_timeout <- 60 * 60
optim_timeout <- 7 * 24 * 60 * 60
num_repeats <- 4


## PROCESS PARAMETER
# # extract the best parameters # almost, needs to be data frames
# best_parms <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets")) %>%
#   mutate(
#     train_score = pmax(0, train_score),
#     test_score = pmax(0, test_score)
#   ) %>%
#   group_by(method_name, fold_i) %>%
#   mutate(norm_score = test_score / mean(test_score)) %>%
#   ungroup() %>%
#   group_by(method_name) %>%
#   arrange(desc(norm_score)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(method_name) %>%
#   mutate(param_group = "best") %>%
#   select(method_name, param_group, design = params)

# extract the default parameters
designs <- lapply(methods$short_name, function(mn) {
  par_set <- methods %>% filter(short_name == mn) %>% .$par_set %>% .[[1]]
  defaults <- ParamHelpers::generateDesignOfDefaults(par_set)
  # best <- ...
  # bind_rows(defaults, best)
  defaults
}) %>% setNames(methods$short_name)


## PROCESS TASKS
# # get the synthetic data
# synthetic_tasks <- readRDS(derived_file("v6/tasks.rds", experiment_id = "datasets/synthetic"))
# synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$settings %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")
#
# # get the real data
# real_names <- list_datasets()
# real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble() %>%
#   mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))
# real_tasks <- real_tasks %>% filter(nrow < 2000) %>% mutate(trajectory_type = unlist(trajectory_type))
#
# # combine tasks
# tasks <- bind_rows(
#   synthetic_tasks %>% mutate(task_group = "synthetic"),
#   real_tasks %>% mutate(task_group = "real")
# )
# tasks <- tasks %>% select(one_of(c("task_group", intersect(colnames(synthetic_tasks), colnames(real_tasks)))))
# write_rds(tasks, derived_file("tasks.rds"))

tasks <- read_rds(derived_file("tasks.rds")) %>%
  rowwise() %>%
  mutate(
    milenet_spr = milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>% list()
  ) %>%
  ungroup()


# # # save benchmark configuration and start it
# write_rds(lst(methods, designs, metrics, extra_metrics, num_repeats, tasks), derived_file("config.rds"))
# benchmark_suite_submit(
#   tasks = tasks,
#   task_group = rep("task", nrow(tasks)),
#   task_fold = rep(1, nrow(tasks)),
#   out_dir = derived_file("suite/"),
#   remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
#   methods = methods %>% filter(!short_name %in% c("ouija", "pseudogp", "GPfates", "topslam")), # these methods did not finish
#   designs = designs,
#   metrics = metrics,
#   extra_metrics = extra_metrics,
#   memory = "20G",
#   num_cores = 4,
#   num_iterations = 1,
#   num_repeats = num_repeats,
#   num_init_params = num_init_params,
#   execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
#   r_module = NULL,
#   output_model = TRUE
# )

outputs <- benchmark_suite_retrieve(derived_file("suite/"))


# process output
outputs2 <- outputs %>%
  rowwise() %>%
  mutate(
    any_errored = any(which_errored),
    memory = ifelse(!is.null(qacct), qacct$maxvmem, NA)
  ) %>%
  ungroup()

# select only the runs that succeeded
succeeded <- outputs2 %>%
  filter(!any_errored) %>%
  group_by(method_name) %>%
  filter(n() == num_repeats) %>%
  ungroup()

tmp <- outputs2 %>%
  rowwise() %>%
  mutate(
    qsub_error = ifelse(qsub_error != "all parameter settings errored", qsub_error, individual_scores$error[[1]]$message),
    qsub_len = str_length(qsub_error),
    qsub_error = str_sub(qsub_error, qsub_len-600, qsub_len)
  ) %>%
  ungroup() %>%
  select(method_name, qsub_error, which_errored) %>%
  filter(which_errored) %>%
  group_by(method_name) %>%
  slice(1) %>%
  ungroup()
for (i in seq_len(nrow(tmp))) {
  cat("METHOD ", tmp$method_name[[i]], " ERRORED BECAUSE OF: \n", sep = "")
  cat(tmp$qsub_error[[i]], "\n\n", sep = "")
}



# bind the metrics of the individual runs
eval_ind <-
  bind_rows(succeeded$individual_scores) %>%
  filter(!method_name %in% c("identity", "shuffle", "random")) %>%
  left_join(tasks %>% select(task_id = id, type, trajectory_type, task_group), by = "task_id") %>%
  mutate(
    param_group = c("default", "optimised")[param_i],
    pct_errored = 1 - sapply(error, is.null),
    prior_sr = sapply(prior_df, function(prdf) ifelse(nrow(prdf) == 0, "", paste(prdf$prior_names, "--", prdf$prior_type, sep = "", collapse = ";"))),
    trajectory_type_f = factor(trajectory_type, levels = dynalysis:::trajectory_type_directed$name)
  ) %>%
  group_by(task_id) %>%
  mutate(
    rank_correlation = percent_rank(correlation),
    rank_rf_mse = percent_rank(-rf_mse),
    rank_rf_rsq = percent_rank(rf_rsq),
    rank_edge_flip = percent_rank(edge_flip)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(error_message = ifelse(is.null(error), "", error$message)) %>%
  ungroup()

# evaluate per replicate
eval_repl <- eval_ind %>%
  group_by(method_name, method_short_name, task_id, fold_type, fold_i, group_sel, param_i, iteration_i, type, trajectory_type, task_group, param_group, prior_sr, trajectory_type_f) %>%
  summarise_if(is.numeric, funs(mean, var)) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation_mean, rank_edge_flip_mean, rank_rf_mse_mean), 1, psych::harmonic.mean)
  )

# process trajtype grouped evaluation
eval_trajtype <- eval_repl %>%
  group_by(method_name, method_short_name, task_group, param_group, trajectory_type, trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# process overall evaluation
eval_overall <- eval_trajtype %>%
  group_by(method_name, method_short_name, task_group, param_group) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# adding totals and means to table
eval_trajtype_wm <- eval_trajtype %>%
  group_by(method_name, method_short_name, param_group, trajectory_type, trajectory_type_f) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_trajtype)

eval_overall_wm <- eval_overall %>%
  group_by(method_name, method_short_name, param_group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_overall)

eval_trajtype_wa_wo <- eval_overall_wm %>%
  mutate(trajectory_type = "overall") %>%
  bind_rows(eval_trajtype_wm) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = c("overall", dynalysis:::trajectory_type_directed$name)))



write_rds(lst(eval_ind, eval_overall, eval_trajtype, eval_repl, eval_trajtype_wa_wo), derived_file("eval_outputs.rds"))



