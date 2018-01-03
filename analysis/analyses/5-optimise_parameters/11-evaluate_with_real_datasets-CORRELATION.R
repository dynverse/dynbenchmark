library(dynalysis)
library(tidyverse)
library(dynplot)
library(PRISM)

experiment("5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION")

# helper function for parameters
trafo_params <- function(parameters, par_set) {
  lapply(names(parameters), function(prnm) {
    parset_par <- par_set$pars[[prnm]]
    parv <- parameters[[prnm]]
    if (!is.null(parset_par$trafo)) {
      parv <- parset_par$trafo(parv)
    }
    parv
  }) %>% setNames(names(parameters))
}

# get the synthetic data
synthetic_tasks <- readRDS(derived_file("v6/tasks.rds", experiment_id = "datasets/synthetic"))
synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$info %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")

# get the real data
real_names <- list_datasets()
real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble() %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))
real_tasks <- real_tasks %>% filter(nrow < 2000) %>% mutate(trajectory_type = unlist(trajectory_type))

# settings
methods <- get_descriptions(as_tibble = F)
metrics <- c("auc_R_nx", "correlation")
timeout <- 300

# extract the best parameters
# best_parms <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets")) %>%
#   mutate(
#     params = mapply(params, method_name, FUN = function(prm, mn) trafo_params(prm, methods[[mn]]$par_set)),
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
#   arrange(method_name)

# extract the default parameters
default_parms <- data_frame(method_name = names(methods), params = lapply(method_name, function(mn) {
  par_set <- methods[[mn]]$par_set
  ParamHelpers::generateDesignOfDefaults(par_set, trafo = TRUE) %>% ParamHelpers::dfRowToList(par.set = par_set, i = 1)
}))

# combine parameter sets
parm_sets <- bind_rows(
  # best_parms %>% select(method_name, params) %>% mutate(param_group = "best"),
  default_parms %>% select(method_name, params) %>% mutate(param_group = "default")
) %>%
  # mutate(output_file = pritt("{method_name}_{param_group}.rds")) %>%
  crossing(replicate = seq_len(4))

# parm_sets <- parm_sets %>% filter(method_name %in% unique(best_parms$method_name))

# combine tasks
# tasks <- bind_rows(
#   synthetic_tasks %>% mutate(task_group = "synthetic"),
#   real_tasks %>% mutate(task_group = "real")
# )
# tasks <- tasks %>% select(one_of(c("task_group", intersect(colnames(synthetic_tasks), colnames(real_tasks)))))
#
# tasks$geodesic_dist <- lapply(seq_len(nrow(tasks)), function(x) list())
# for (i in seq_len(nrow(tasks))) {
#   cat(i, "/", nrow(tasks), "\n", sep="")
#   task <- extract_row_to_list(tasks, i)
#   tasks$geodesic_dist[[i]] <- dynutils::compute_emlike_dist(task)
# }
# for (i in seq_len(nrow(tasks))) {
#   cat(i, "/", nrow(tasks), "\n", sep="")
#   expression <- tasks$expression[[i]]
#   cell_ids <- rownames(expression)[apply(expression, 1, function(x) length(unique(x)) > 1)]
#   tasks$cell_ids[[i]] <- cell_ids
#   tasks$expression[[i]] <- tasks$expression[[i]][cell_ids,]
#   tasks$counts[[i]] <- tasks$counts[[i]][cell_ids,]
#   tasks$milestone_percentages[[i]] <- tasks$milestone_percentages[[i]] %>% filter(cell_id %in% cell_ids)
#   tasks$progressions[[i]] <- tasks$progressions[[i]] %>% filter(cell_id %in% cell_ids)
#   tasks$cell_info[[i]] <- tasks$cell_info[[i]] %>% filter(cell_id %in% cell_ids)
#   tasks$geodesic_dist[[i]] <- tasks$geodesic_dist[[i]][cell_ids, cell_ids]
#   tasks$prior_information[[i]] <- dynutils::generate_prior_information(
#     milestone_ids = tasks$milestone_ids[[i]],
#     milestone_network = tasks$milestone_network[[i]],
#     progressions = tasks$progressions[[i]],
#     milestone_percentages = tasks$milestone_percentages[[i]],
#     counts = tasks$counts[[i]],
#     feature_info = tasks$feature_info[[i]],
#     cell_info = tasks$cell_info[[i]]
#   )
# }
# for (i in seq_len(nrow(tasks))) {
#   tasks$trajectory_type[[i]] <- dynutils::classify_milestone_network(tasks$milestone_network[[i]])$network_type
# }

# write_rds(tasks, derived_file("tasks.rds"))
tasks <- read_rds(derived_file("tasks.rds"))

run_fun <- function(i) {
  method_name <- parm_sets$method_name[[i]]
  param_group <- parm_sets$param_group[[i]]
  parameters <- parm_sets$params[[i]]
  replicate <- parm_sets$replicate[[i]]

  cat(pritt("Running {i}/{nrow(parm_sets)}: {method_name}--{param_group}--{replicate}\n\n"))
  method <- methods[[method_name]]

  score <- execute_evaluation(
    tasks = tasks,
    method = method,
    parameters = parameters,
    metrics = metrics,
    timeout = timeout,
    output_model = T,
    error_score = 0,
    mc_cores = 8
  )

  extras <- attr(score, "extras")
  models <- extras$.models
  summary <- extras$.summary
  attr(score, "extras") <- NULL
  lst(method_name, param_group, replicate, parameters, score, models, summary)
}

parm_sets <- parm_sets %>% filter(!method_name %in% c("GPfates", "Mpath", "ouija", "pseudogp", "SCOUP"))
# rerun pseudogp with fewer cores

# run everything locally
filenames <- lapply(seq_len(nrow(parm_sets)), function(i) {
  method_name <- parm_sets$method_name[[i]]
  param_group <- parm_sets$param_group[[i]]
  replicate <- parm_sets$replicate[[i]]

  filename <- derived_file(pritt("out_rds_{method_name}_{param_group}_{replicate}.rds"))
  if (!file.exists(filename)) {
    out <- run_fun(i)
    write_rds(out, filename)
  }
  filename
})

outs <- pbapply::pblapply(filenames, read_rds)

# # run everything on the cluster
# qsub_handle <- qsub_lapply(
#   X = seq_len(nrow(parm_sets)),
#   qsub_config = override_qsub_config(
#     name = "dynreal",
#     num_cores = 8,
#     memory = "10G",
#     max_wall_time = NULL,
#     remove_tmp_folder = FALSE,
#     stop_on_error = FALSE,
#     verbose = FALSE,
#     execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
#     r_module = NULL,
#     wait = FALSE
#   ),
#   qsub_packages = c("dplyr", "purrr", "dynalysis", "mlrMBO", "parallelMap"),
#   qsub_environment = c("parm_sets", "tasks", "methods", "metrics", "timeout"),
#   FUN = run_fun
# )
#
# # write_rds(qsub_handle, derived_file("qsub_handle"))
# qsub_handle <- read_rds(derived_file("qsub_handle"))
#
# outs <- qsub_retrieve(qsub_handle)

# process data
trajtype_ord <- c("directed_linear", "directed_cycle", "bifurcation", "multifurcation", "rooted_tree", "directed_acyclic_graph", "directed_graph")

eval_ind <- map_df(outs, function(output) {
  summary <- output$summary %>% left_join(tasks %>% select(task_id = id, task_group, trajectory_type), by = "task_id")
  summary$replicate <- output$replicate
  summary$param_group <- output$param_group
  summary$parameters <- list(output$parameters)
  summary$model <- output$models
  summary %>%
    select(method_name, method_short_name, task_id, task_group, param_group, parameters, model, correlation, everything()) %>%
    mutate(
      percentage_errored = 1 - is.null(error),
      prior_str = sapply(prior_df, function(prdf) ifelse(nrow(prdf) == 0, "", paste(prdf$prior_names, "--", prdf$prior_type, sep = "", collapse = ";"))),
      trajectory_type_f = factor(trajectory_type, levels = trajtype_ord)
    )
}) %>%
  filter(!method_short_name %in% c("identity", "random", "shuffle")) %>%
  group_by(task_id) %>%
  mutate(rank_correlation = percent_rank(correlation)) %>%
  ungroup()

# process trajtype grouped evaluation
eval_trajtype <- eval_ind %>%
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

# get ordering of methods
method_ord <- eval_overall %>%
  filter(task_group == "real") %>%
  group_by(method_name) %>%
  arrange(desc(rank_correlation)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(rank_correlation)) %>%
  .$method_name

eval_overall <- eval_overall %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_ind <- eval_ind %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_trajtype <- eval_trajtype %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))

# evaluate per replicate
eval_repl <- eval_ind %>%
  group_by(method_name, method_name_f, method_short_name, task_group, param_group, replicate) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

write_rds(lst(eval_ind, eval_overall, eval_trajtype, eval_repl), derived_file("eval_outputs.rds"))
# list2env(read_rds(derived_file("eval_outputs.rds")), environment())
