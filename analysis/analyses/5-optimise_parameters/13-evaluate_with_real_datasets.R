library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/13-evaluate_with_real_datasets")

# # get the synthetic data
# synthetic_tasks <- readRDS(derived_file("v6/tasks.rds", experiment_id = "datasets/synthetic"))
# synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$info %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")
#
# # get the real data
# real_names <- list_datasets()
# real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble() %>%
#   mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))
# real_tasks <- real_tasks %>% filter(nrow < 2000) %>% mutate(trajectory_type = unlist(trajectory_type))

# settings
methods <- get_descriptions()
metrics <- c("correlation")
extra_metrics <- c("rf_mse")#, "edge_flip")
timeout <- 60 * 60
num_repeats <- 4

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

# # combine tasks
# tasks <- bind_rows(
#   synthetic_tasks %>% mutate(task_group = "synthetic"),
#   real_tasks %>% mutate(task_group = "real")
# )
# tasks <- tasks %>% select(one_of(c("task_group", intersect(colnames(synthetic_tasks), colnames(real_tasks)))))

tasks <- read_rds(derived_file("tasks.rds"))

tasks <- tasks %>% rowwise() %>% mutate(
  milenet_spr = milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>% list()
) %>% ungroup()

write_rds(lst(methods, designs, metrics, extra_metrics, num_repeats, timeout), derived_file("config.rds"))

# start benchmark
benchmark_suite_submit(
  tasks = tasks,
  task_group = rep("task", nrow(tasks)),
  task_fold = rep(1, nrow(tasks)),
  out_dir = derived_file("suite/"),
  remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
  methods = methods,
  designs = designs,
  metrics = metrics,
  extra_metrics = extra_metrics,
  timeout = timeout,
  memory = "11G",
  num_cores = 1,
  num_iterations = 1,
  num_repeats = num_repeats,
  num_init_params = num_init_params,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL,
  output_model = TRUE
)

outputs <- benchmark_suite_retrieve(derived_file("suite/"))


# process output
outputs2 <- outputs %>%
  rowwise() %>%
  mutate(
    any_errored = any(unlist(which_errored)),
    memory = ifelse(!is.null(qacct), qacct$maxvmem, NA)
  ) %>%
  ungroup()

# select only the runs that succeeded
succeeded <- outputs2 %>%
  filter(!any_errored) %>%
  group_by(method_name) %>%
  filter(n() == num_repeats) %>%
  ungroup()

# bind the metrics of the individual runs
eval_ind <-
  bind_rows(succeeded$individual_scores) %>%
  left_join(tasks %>% select(task_id = id, type, trajectory_type), by = "task_id") %>%
  mutate(pct_errored = 1 - sapply(error, is.null))

# priors <- eval_ind %>%
#   group_by(method_name) %>%
#   slice(1) %>%
#   rowwise() %>%
#   mutate(prior = ifelse(nrow(prior_df) == 0, "", paste(prior_df$prior_names, "--", prior_df$prior_type, sep = "", collapse = ";"))) %>%
#   ungroup() %>%
#   select(method_name, prior, prior_df)

