library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

# # helper function for parameters
# trafo_params <- function(parameters, par_set) {
#   lapply(names(parameters), function(prnm) {
#     parset_par <- par_set$pars[[prnm]]
#     parv <- parameters[[prnm]]
#     if (!is.null(parset_par$trafo)) {
#       parv <- parset_par$trafo(parv)
#     }
#     parv
#   }) %>% setNames(names(parameters))
# }
#
# # get the synthetic data
# synthetic_tasks <- readRDS(derived_file("v5.rds", experiment_id = "datasets/synthetic"))
# for (i in seq_len(nrow(synthetic_tasks))) {
#   synthetic_tasks$trajectory_type[[i]] <- dynutils::classify_milestone_network(synthetic_tasks$milestone_network[[i]])$network_type
# }
# synthetic_tasks <- synthetic_tasks %>% left_join(synthetic_tasks$info %>% map_df(as_data_frame) %>% mutate(id = synthetic_tasks$id), by = "id")
#
# # get the real data
# real_names <- list_datasets()
# real_tasks <- pbapply::pblapply(real_names, load_dataset) %>% list_as_tibble() %>%
#   mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))
# real_tasks <- real_tasks %>% filter(nrow < 2000)
#
# # settings
# methods <- get_descriptions(as_tibble = F)
# metrics <- "auc_R_nx"
# timeout <- 300
#
# # extract the best parameters
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
#
# # extract the default parameters
# default_parms <- data_frame(method_name = names(methods), params = lapply(method_name, function(mn) {
#   par_set <- methods[[mn]]$par_set
#   ParamHelpers::generateDesignOfDefaults(par_set, trafo = TRUE) %>% ParamHelpers::dfRowToList(par.set = par_set, i = 1)
# }))
#
# # combine parameter sets
# parm_sets <- bind_rows(
#   best_parms %>% select(method_name, params) %>% mutate(param_group = "best"),
#   default_parms %>% select(method_name, params) %>% mutate(param_group = "default")
# ) %>% mutate(output_file = pritt("{method_name}_{param_group}.rds"))
#
# parm_sets <- parm_sets %>% filter(method_name %in% unique(best_parms$method_name))
#
# # combine tasks
# tasks <- bind_rows(
#   synthetic_tasks %>% mutate(task_group = "synthetic"),
#   real_tasks %>% mutate(task_group = "real")
# )
# tasks <- tasks %>% select(one_of(c("task_group", intersect(colnames(synthetic_tasks), colnames(real_tasks)))))
#
# # run everything
# for (i in seq_len(nrow(parm_sets))) {
#   output_file <- derived_file(parm_sets$output_file[[i]])
#
#   if (!file.exists(output_file)) {
#     method_name <- parm_sets$method_name[[i]]
#     param_group <- parm_sets$param_group[[i]]
#     parameters <- parm_sets$params[[i]]
#     cat(pritt("Running {method_name}--{param_group}\n\n"))
#     method <- methods[[method_name]]
#
#     score <- execute_evaluation(
#       tasks = tasks,
#       method = method,
#       parameters = parameters,
#       metrics = metrics,
#       timeout = timeout,
#       output_model = T,
#       error_score = 0,
#       mc_cores = 8
#     )
#
#     extras <- attr(score, "extras")
#     models <- extras$.models
#     summary <- extras$.summary
#     attr(score, "extras") <- NULL
#     write_rds(lst(method_name, param_group, parameters, score, models, summary), output_file)
#   }
# }
#
# # process data
# trajtype_ord <- c("directed_linear", "directed_cycle", "bifurcation", "multifurcation", "rooted_tree", "directed_acyclic_graph", "directed_graph")
#
# eval_ind <- map_df(seq_len(nrow(parm_sets)), function(i) {
#   output_file <- derived_file(parm_sets$output_file[[i]])
#   output <- read_rds(output_file)
#   summary <- output$summary %>% left_join(tasks %>% select(task_id = id, task_group, trajectory_type), by = "task_id")
#   summary$param_group <- output$param_group
#   summary$parameters <- list(output$parameters)
#   summary$model <- output$models
#   summary %>%
#     select(method_name, method_short_name, task_id, task_group, param_group, parameters, model, auc_R_nx, everything()) %>%
#     mutate(
#       percentage_errored = 1 - is.null(error),
#       prior_str = sapply(prior_df, function(prdf) ifelse(nrow(prdf) == 0, "", paste(prdf$prior_names, "--", prdf$prior_type, sep = "", collapse = ";"))),
#       trajectory_type_f = factor(trajectory_type, levels = trajtype_ord)
#     )
# }) %>% filter(!method_short_name %in% c("identity", "random", "shuffle"))
#
# eval_ind %>% group_by(method_short_name, prior_str) %>% summarise(n=n()) %>% ungroup
#
# # process overal evaluation
# eval_overall <- eval_ind %>%
#   group_by(method_name, method_short_name, task_group, param_group) %>%
#   mutate(n = n()) %>%
#   summarise_if(is.numeric, mean) %>%
#   ungroup()
#
# method_ord <- eval_overall %>% filter(task_group == "real", param_group == "best") %>% arrange(desc(auc_R_nx)) %>% .$method_name
#
# eval_overall <- eval_overall %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
#
#
# # process trajtype grouped evaluation
# eval_trajtype <- eval_ind %>%
#   group_by(method_name, method_short_name, task_group, param_group, trajectory_type, trajectory_type_f) %>%
#   mutate(n = n()) %>%
#   summarise_if(is.numeric, mean) %>%
#   ungroup() %>%
#   mutate(
#     method_name_f = factor(method_name, levels = rev(method_ord))
#   )
#
# write_rds(lst(eval_ind, eval_overall, eval_trajtype), derived_file("eval_outputs.rds"))

source("analysis/analyses/4-method_characterization/0_common.R")

evals <- read_rds(derived_file("eval_outputs.rds", "5-optimise_parameters/8-evaluate_with_real_datasets"))
list2env(evals, environment())
method_df_evaluated <- read_rds(derived_file("method_df_evaluated.rds", "method_characteristics")) %>%
  rowwise() %>%
  do({
    df <- .
    method_short_name <- strsplit(df$ids, ",")[[1]]
    data.frame(df, method_short_name, stringsAsFactors = FALSE, check.names = FALSE)
  })


joined <- eval_overall %>% left_join(method_df_evaluated, by = "method_short_name") %>% filter(method_short_name != "ouija")

besttwo <- joined %>% group_by(method_short_name, task_group) %>% arrange(desc(auc_R_nx)) %>% slice(1) %>% ungroup() %>% filter(task_group == "real")
line <- besttwo %>%
  filter(name %in% c("Monocle 1", "Waterfall", "embeddr", "SLICER", "TSCAN", "slingshot")) %>%
  add_row(date = Sys.time(), auc_R_nx = max(besttwo$auc_R_nx)) %>%
  add_row(date = "2014-01-01", auc_R_nx = 0) %>%
  arrange(date)



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

pdf(figure_file("1_overall_comparison_best.pdf"), 12, 4)
z <- joined %>% group_by(method_short_name, task_group) %>% arrange(desc(auc_R_nx)) %>% slice(1) %>% ungroup()
zmethord <- z %>% filter(task_group == "real") %>% arrange(desc(auc_R_nx)) %>% .$method_name
z <- z %>% mutate(method_name_f = factor(method_name, levels = rev(zmethord)))
g <- cowplot::plot_grid(plotlist = lapply(c("real", "synthetic"), function(tg) {
  ggplot(z %>% filter(task_group == tg)) +
    geom_point(aes(method_name_f, auc_R_nx)) +
    coord_flip() +
    cowplot::theme_cowplot() +
    labs(x = NULL, title = pritt("Scores on {tg} datasets"), colour = "Parameter\ngroup")
}), nrow = 1)
print(g)
dev.off()

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
#

# g1 <- ggplot(joined %>% filter(task_group == "real", param_group == "best"), aes(date, auc_R_nx)) +
#   geom_smooth(span=2) +
#   geom_point() +
#   ggrepel::geom_label_repel(aes(label = name)) +
#   cowplot::theme_cowplot() +
#   labs(x = "Time", y = "auc_R_nx score", title = "Overall performance with optimised parameters")
#
# g2 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(date, auc_R_nx)) +
#   geom_smooth(span=2) +
#   geom_point() +
#   ggrepel::geom_label_repel(aes(label = name)) +
#   cowplot::theme_cowplot() +
#   labs(x = "Time", y = "auc_R_nx score", title = "Overall performance with optimised parameters")
#
# g3 <- ggplot(joined %>% group_by(method_short_name, task_group) %>% arrange(desc(auc_R_nx)) %>% slice(1) %>% ungroup() %>% filter(task_group == "real"), aes(date, auc_R_nx)) +
#   geom_smooth(span=2) +
#   geom_point() +
#   ggrepel::geom_label_repel(aes(label = name)) +
#   cowplot::theme_cowplot() +
#   labs(x = "Time", y = "auc_R_nx score", title = "Overall performance with optimised parameters")
#
# cowplot::plot_grid(g1, g2, g3, nrow = 1)

# compare best vs. default
pdf(figure_file("best_vs_default.pdf"), 12, 8)
ggplot(joined %>% filter(task_group == "real"), aes(date, auc_R_nx)) +
  geom_smooth(span=2) +
  geom_path(aes(group = name), linetype = "dashed") +
  geom_point(aes(colour = param_group)) +
  ggrepel::geom_label_repel(aes(label = name, colour = param_group)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "auc_R_nx score", title = "Overall performance with optimised parameters")
dev.off()


# multiple plots
g1 <- ggplot(method_df_evaluated, aes(date, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
pdf(figure_file("qc_over_time.pdf"), 12, 8)
g1
dev.off()

g2 <- ggplot(method_df_evaluated, aes(date, Citations+1)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_log10() +
  labs(x = "Time", y = "Citations", title = "Citations over time")
pdf(figure_file("citations_over_time.pdf"), 12, 8)
g2
dev.off()

g3 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(date, auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (auc_R_nx)", title = "Overall performance with optimised parameters") +
  theme(legend.position = "bottom")
pdf(figure_file("performance_over_time.pdf"), 12, 8)
g3
dev.off()

g4 <- ggplot(method_df_evaluated, aes(Citations+1, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "QC score", title = "QC score over # citations")
pdf(figure_file("qc_over_citations.pdf"), 12, 8)
g4
dev.off()

g5 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(Citations+1, auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "Performance (auc_R_nx)", title = "auc_R_nx score over # citations")
pdf(figure_file("performance_over_citations.pdf"), 12, 8)
g5
dev.off()

g6 <- ggplot(joined %>% filter(task_group == "real", param_group == "default"), aes(qc_score, auc_R_nx)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "QC score", y = "Score", title = "QC score over auc_R_nx score")
pdf(figure_file("performance_over_qc.pdf"), 12, 8)
g6
dev.off()

pdf(figure_file("qc_vs_time_vs_aucRnx.pdf"), 18, 18)
print(cowplot::plot_grid(g1, g2, g3, g4, g5, g6, ncol = 2, align = "hv"))
dev.off()


# best
pdf(figure_file("bestparam_over_time.pdf"), 12, 8)
ggplot(besttwo, aes(date, auc_R_nx)) +
  geom_step(data = line, linetype = "dashed", colour = "gray") +
  geom_point() +
  # ggrepel::geom_label_repel(aes(label = name)) +
  geom_text(aes(label = name), nudge_y = .005) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "Performance (auc_R_nx)") +
  scale_y_continuous(limits = c(0, max(besttwo$auc_R_nx)+.01), breaks = c(0, .05, .1, round(max(besttwo$auc_R_nx), 2)))
dev.off()


ggplot(method_df_evaluated, aes(date, qc_score, colour = trajectory_type, group = 1)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")+
  scale_colour_manual(values = trajectory_type_colors)
