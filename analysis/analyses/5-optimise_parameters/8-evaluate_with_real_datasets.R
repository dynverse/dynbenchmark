library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

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
synthetic_tasks <- readRDS(derived_file("v5.rds", experiment_id = "datasets/synthetic"))
for (i in seq_len(nrow(synthetic_tasks))) {
  synthetic_tasks$trajectory_type[[i]] <- dynutils::classify_milestone_network(synthetic_tasks$milestone_network[[i]])$network_type
}
synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$info %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")

# get the real data
real_names <- list_datasets()
real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble() %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))
real_tasks <- real_tasks %>% filter(nrow < 2000)

# settings
methods <- get_descriptions(as_tibble = F)
metrics <- "auc_R_nx"
timeout <- 300

# extract the best parameters
best_parms <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets")) %>%
  mutate(
    params = mapply(params, method_name, FUN = function(prm, mn) trafo_params(prm, methods[[mn]]$par_set)),
    train_score = pmax(0, train_score),
    test_score = pmax(0, test_score)
  ) %>%
  group_by(method_name, fold_i) %>%
  mutate(norm_score = test_score / mean(test_score)) %>%
  ungroup() %>%
  group_by(method_name) %>%
  arrange(desc(norm_score)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(method_name)

# extract the default parameters
default_parms <- data_frame(method_name = names(methods), params = lapply(method_name, function(mn) {
  par_set <- methods[[mn]]$par_set
  ParamHelpers::generateDesignOfDefaults(par_set, trafo = TRUE) %>% ParamHelpers::dfRowToList(par.set = par_set, i = 1)
}))

# combine parameter sets
parm_sets <- bind_rows(
  best_parms %>% select(method_name, params) %>% mutate(param_group = "best"),
  default_parms %>% select(method_name, params) %>% mutate(param_group = "default")
) %>% mutate(output_file = pritt("{method_name}_{param_group}.rds"))

parm_sets <- parm_sets %>% filter(method_name %in% unique(best_parms$method_name))

# combine tasks
tasks <- bind_rows(
  synthetic_tasks %>% mutate(task_group = "synthetic"),
  real_tasks %>% mutate(task_group = "real")
)
tasks <- tasks %>% select(one_of(c("task_group", intersect(colnames(synthetic_tasks), colnames(real_tasks)))))

# run everything
for (i in seq_len(nrow(parm_sets))) {
  output_file <- derived_file(parm_sets$output_file[[i]])

  if (!file.exists(output_file)) {
    method_name <- parm_sets$method_name[[i]]
    param_group <- parm_sets$param_group[[i]]
    parameters <- parm_sets$params[[i]]
    cat(pritt("Running {method_name}--{param_group}\n\n"))
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
    write_rds(lst(method_name, param_group, parameters, score, models, summary), output_file)
  }
}

# process data
trajtype_ord <- c("directed_linear", "directed_cycle", "bifurcation", "multifurcation", "rooted_tree", "directed_acyclic_graph", "directed_graph")

eval_ind <- map_df(seq_len(nrow(parm_sets)), function(i) {
  output_file <- derived_file(parm_sets$output_file[[i]])
  output <- read_rds(output_file)
  summary <- output$summary %>% left_join(tasks %>% select(task_id = id, task_group, trajectory_type), by = "task_id")
  summary$param_group <- output$param_group
  summary$parameters <- list(output$parameters)
  summary$model <- output$models
  summary %>%
    select(method_name, method_short_name, task_id, task_group, param_group, parameters, model, auc_R_nx, everything()) %>%
    mutate(
      percentage_errored = 1 - is.null(error),
      prior_str = sapply(prior_df, function(prdf) ifelse(nrow(prdf) == 0, "", paste(prdf$prior_names, "--", prdf$prior_type, sep = "", collapse = ";"))),
      trajectory_type_f = factor(trajectory_type, levels = trajtype_ord)
    )
}) %>% filter(!method_short_name %in% c("identity", "random", "shuffle"))

eval_ind %>% group_by(method_short_name, prior_str) %>% summarise(n=n()) %>% ungroup

# process overal evaluation
eval_overall <- eval_ind %>%
  group_by(method_name, method_short_name, task_group, param_group) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

method_ord <- eval_overall %>% filter(task_group == "real", param_group == "best") %>% arrange(desc(auc_R_nx)) %>% .$method_name

eval_overall <- eval_overall %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))

pdf(figure_file("1_overall_comparison.pdf"), 12, 4)
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(eval_overall %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, auc_R_nx, colour = param_group)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()

# process trajtype grouped evaluation
eval_trajtype <- eval_ind %>%
  group_by(method_name, method_short_name, task_group, param_group, trajectory_type, trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    method_name_f = factor(method_name, levels = rev(method_ord))
  )

pdf(figure_file("2_trajtype_comparison.pdf"), 12, 16)
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(eval_trajtype %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, auc_R_nx, colour = param_group)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    facet_wrap(~trajectory_type_f, ncol = 1) +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()

write_rds(lst(eval_ind, eval_overall, eval_trajtype), derived_file("eval_outputs.rds"))
