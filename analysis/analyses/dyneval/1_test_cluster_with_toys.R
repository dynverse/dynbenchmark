library(dynalysis)
library(tidyverse)
library(cowplot)

derived_dir <- "analysis/data/derived_data/dyneval/1_test_cluster_with_toys/"

# remove previous output
# unlink(derived_dir, recursive=TRUE)

dir.create(derived_dir, recursive = T)

# easy test
tasks_file <- paste0(derived_dir, "tasks.RData")
if (file.exists(tasks_file)) {
  load(tasks_file)
} else {
  tasks <- generate_toy_datasets(num_replicates = 2)
  task_group <- rep("group", nrow(tasks))
  task_fold <- gsub(".*_", "", tasks$id) %>% as.integer()
  save(tasks, task_group, task_fold, file = tasks_file)
}
methods <- get_descriptions(as_tibble = T)

metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation",
             "isomorphic", "ged", "robbie_network_score", "mantel_pval")
# start benchmark suite
benchmark_suite_submit(
  tasks,
  task_group,
  task_fold,
  out_dir = derived_dir,
  save_r2g_to_outdir = TRUE,
  methods = methods,
  metrics = metrics,
  timeout = 600,
  memory = "16G",
  num_cores = 2,
  num_iterations = 5,
  num_init_params = 16
)

outputs <- benchmark_suite_retrieve(derived_dir)

failed <- outputs %>% filter(which_errored, error != "job is still running")

failed
out <- failed %>% extract_row_to_list(5)
out$method_name
cat(out$error)
out$qacct

# GPfates: teveel geheugen
# monocPQ: Evaluation error: invalid 'type' (builtin) of argument.
# slgnsht: could not find function "smootherFcn"


##################################################
# SLICE:
# [mbo] 0: lm.method=clustering; model.type=graph; ss.method=pcst; ss.threshold=0.0433; community.method=label_prop; cluster.method=kmeans; k=5; k.max=14; B=458; k.opt.method=firstSEmax : y_1 = 0.495, y_2 = 0.244 : 192.3 secs : initdesign
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: k
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: k
# Error in predict.randomForest(.model$learner.model, newdata = .newdata,  :
#   missing values in newdata
# Calls: do.call ... predictLearner.regr.randomForest -> predict -> predict.randomForest
# Execution halted

##################################################
# StemID:
# [mbo] 0: clustnr=56; bootnr=74; metric=euclidean; num_cluster_method=sat; SE.method=globalmax; SE.factor=0.491; B.gap=79; cln=43; FUNcluster=kmeans; dimred_method=tsne; outminc=47; outlg=24; probthr=0.00479; thr_lower=-68.1; thr_upper=-66.5; outdistquant=0.736; nmode=FALSE; pdishuf=236; pthr=0.0397; pethr=0.00205 : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: metric
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: metric
# Error in predict.randomForest(.model$learner.model, newdata = .newdata,  :
#   missing values in newdata

##################################################
# topslam (lijkt wel een beetje te werken):
# [mbo] 0: n_components=2; n_neighbors=10; linear_dims=0; max_iters=200; dimreds=TRUE,TRUE,TRUE,TRUE,TRUE : y_1 = 0.683, y_2 = 0.467 : 376.2 secs : initdesign
# [mbo] 0: n_components=3; n_neighbors=4; linear_dims=4; max_iters=916; dimreds=TRUE,TRUE,TRUE,FALSE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=10; n_neighbors=79; linear_dims=5; max_iters=252; dimreds=FALSE,FALSE,TRUE,FALSE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=2; n_neighbors=72; linear_dims=1; max_iters=332; dimreds=TRUE,FALSE,TRUE,FALSE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=7; n_neighbors=33; linear_dims=3; max_iters=697; dimreds=TRUE,TRUE,TRUE,FALSE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=6; n_neighbors=24; linear_dims=1; max_iters=201; dimreds=FALSE,FALSE,FALSE,TRUE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=3; n_neighbors=31; linear_dims=3; max_iters=442; dimreds=FALSE,FALSE,TRUE,TRUE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=5; n_neighbors=15; linear_dims=2; max_iters=286; dimreds=FALSE,FALSE,FALSE,TRUE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=2; n_neighbors=60; linear_dims=3; max_iters=752; dimreds=TRUE,FALSE,FALSE,FALSE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=10; n_neighbors=46; linear_dims=4; max_iters=585; dimreds=TRUE,TRUE,FALSE,FALSE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=5; n_neighbors=96; linear_dims=0; max_iters=991; dimreds=FALSE,TRUE,FALSE,FALSE,TRUE : y_1 = 0.78, y_2 = 0.467 : 283.0 secs : initdesign
# [mbo] 0: n_components=6; n_neighbors=94; linear_dims=2; max_iters=544; dimreds=FALSE,TRUE,TRUE,TRUE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=8; n_neighbors=56; linear_dims=2; max_iters=136; dimreds=TRUE,FALSE,TRUE,TRUE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=9; n_neighbors=66; linear_dims=1; max_iters=831; dimreds=FALSE,TRUE,TRUE,TRUE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=4; n_neighbors=87; linear_dims=0; max_iters=424; dimreds=TRUE,FALSE,FALSE,TRUE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=7; n_neighbors=8; linear_dims=4; max_iters=608; dimreds=TRUE,TRUE,FALSE,TRUE,TRUE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# [mbo] 0: n_components=9; n_neighbors=45; linear_dims=5; max_iters=873; dimreds=FALSE,TRUE,FALSE,FALSE,FALSE : y_1 = -1, y_2 = -1 : NA secs (imputed) : initdesign
# Error in sample.int(length(x), size, replace, prob) :
#   invalid first argument
# Calls: do.call ... infill.opt.fun -> lapply -> FUN -> sample -> sample.int

##################################################
# TSCAN:
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: modelNames
# Warning in makeTask(type = type, data = data, weights = weights, blocking = blocking,  :
#   Empty factor levels were dropped for columns: modelNames
# Error in predict.randomForest(.model$learner.model, newdata = .newdata,  :
#   missing values in newdata

##################################################
# TSCAN (bij veel van de parameters): a dimension is zero
# Voorbeeld: minexpr_percent=0.559; minexpr_value=2.88; cvcutoff=3.03; exprmclust_clusternum_lower=13; exprmclust_clusternum_upper=14; modelNames=EEE
##################################################

# ouija (veel verschillende errors):
#
# * timeouts
# * dimension is zero
# Error : a dimension is zero
# * sigpipe
# Error in serialize(data, node$con) : ignoring SIGPIPE signal
# Calls: <Anonymous> ... doTryCatch -> sendData -> sendData.SOCKnode -> serialize
# 3: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low
# 4: Examine the pairs() plot to diagnose sampling problems
#
# 5: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low
# 6: Examine the pairs() plot to diagnose sampling problems
#
# 7: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low
# 8: Examine the pairs() plot to diagnose sampling problems
#
# Execution halted
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//RtmpAp1Mfk': Directory not empty
# Error in igraph::distances(., v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length) :
#   At structural_properties.c:5305 : cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//RtmpBDGdKy': Directory not empty
# Error in igraph::distances(., v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length) :
#   At structural_properties.c:5305 : cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//RtmpagblIh': Directory not empty
# Error in igraph::distances(., v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length) :
#   At structural_properties.c:5305 : cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//RtmpeISzKR': Directory not empty
# Error : a dimension is zero
# Error : a dimension is zero
# Error in igraph::distances(., v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length) :
#   At structural_properties.c:5305 : cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//RtmpS9KJfK': Directory not empty
# Error : a dimension is zero
# Error in igraph::distances(., v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length) :
#   At structural_properties.c:5305 : cannot run Bellman-Ford algorithm, Negative loop detected while calculating shortest paths
# rm: cannot remove `/scratch/irc/personal/robrechtc/tmp//Rtmp3Ktejg': Directory not empty
# Error : a dimension is zero
# Error : a dimension is zero
# Error in cancel_output_fun(time_waited) :
#   Timeout after 600.664764165878 seconds
# Error : a dimension is zero
# Error : a dimension is zero
# Error in cancel_output_fun(time_waited) :
#   Timeout after 600.336585998535 seconds

failed %>% filter(error != "job is still running") %>% extract_row_to_list(7) %>% .$error %>% cat
failed %>% filter(error != "job is still running") %>% extract_row_to_list(7) %>% .$method_name

# succeeded <- outputs %>% filter(!which_errored)
#
# time_df <- bind_rows(succeeded$eval_ind) %>% select(method_name, starts_with("time_")) %>% group_by(method_name) %>% summarise_all(mean) %>%
#   gather(part, time, -method_name) %>% mutate(part = gsub("time_", "", part))
#
# method_name_u <- time_df$method_name %>% unique %>% sort(decreasing = T)
# pdf(paste0(derived_dir, "/results_timings.pdf"), 10, 10)
# ggplot(time_df %>% mutate(method_name = factor(method_name, levels = method_name_u))) +
#   geom_bar(aes(method_name, time, fill = method_name), stat = "identity") +
#   facet_wrap(~part, scales = "free") +
#   coord_flip() +
#   theme(legend.position = "none") +
#   labs(title = "Timings on toy datasets")
# dev.off()
#
# eval_grp <- bind_rows(succeeded$eval_grp) %>% mutate(harmonicmean = 2 * correlation * robbie_network_score / (correlation + robbie_network_score))
# param_sels <- eval_grp %>%
#   filter(fold_type == "train") %>%
#   group_by(ti_type, task_group, method_name, fold_i) %>%
#   arrange(desc(harmonicmean)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(ti_type, task_group, method_name, fold_i, param_i)
# train_eval_grp <- eval_grp %>% filter(fold_type == "train") %>% inner_join(param_sels, by = colnames(param_sels))
# test_eval_grp <- eval_grp %>% filter(fold_type == "test") %>% inner_join(param_sels, by = colnames(param_sels))
#
# train_eval_grp_summ <- train_eval_grp %>% group_by(ti_type, task_group, method_name) %>% summarise_if(is.numeric, mean) %>% ungroup
# test_eval_grp_summ <- test_eval_grp %>% group_by(ti_type, task_group, method_name) %>% summarise_if(is.numeric, mean) %>% ungroup
#
# ordered_names <- test_eval_grp_summ %>% group_by(method_name) %>% summarise_if(is.numeric, mean) %>% ungroup() %>% arrange(harmonicmean) %>% .$method_name
# train_eval_grp_summ <- train_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))
# test_eval_grp_summ <- test_eval_grp_summ %>% mutate(method_name_f = factor(method_name, levels = ordered_names))
#
# g <- cowplot::plot_grid(
#   ggplot(test_eval_grp_summ) +
#     geom_bar(aes(method_name_f, correlation, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   ggplot(test_eval_grp_summ) +
#     geom_bar(aes(method_name_f, robbie_network_score, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   ggplot(test_eval_grp_summ) +
#     geom_bar(aes(method_name_f, harmonicmean, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   nrow = 1
# )
#
# pdf(paste0(derived_dir, "/results_metrics_test.pdf"), 15, 15)
# g
# dev.off()
#
# g <- cowplot::plot_grid(
#   ggplot(train_eval_grp_summ) +
#     geom_bar(aes(method_name_f, correlation, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   ggplot(train_eval_grp_summ) +
#     geom_bar(aes(method_name_f, robbie_network_score, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   ggplot(train_eval_grp_summ) +
#     geom_bar(aes(method_name_f, harmonicmean, fill = method_name_f), stat = "identity") +
#     facet_wrap(~ti_type, ncol = 1) +
#     coord_flip() +
#     theme(legend.position = "none"),
#   nrow = 1
# )
#
# pdf(paste0(derived_dir, "/results_metrics_train.pdf"), 15, 15)
# g
# dev.off()
