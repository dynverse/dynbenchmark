library(dynbenchmark)
library(tidyverse)

experiment("02-metrics/optimise_feature_importance")

set.seed(1)

###############################################################
###                 DETERMINE ORIGINAL IMPS                 ###
###############################################################
if (!file.exists(result_file("fimp_orig.rds"))) {
  num_repeats <- 10

  dataset_ids <- list_datasets() %>% sample_n(10) %>% pull(id)
  # dataset_ids <- select_platforms(n_platforms = 5) %>% map_chr(~ .$platform_id)
  datasets <- load_datasets(ids = dataset_ids, as_tibble = FALSE) %>% set_names(dataset_ids)

  dataset_metadata <-
    crossing(
      dataset_id = dataset_ids,
      repeat_ix = seq_len(num_repeats)
    ) %>%
    pmap_df(function(dataset_id, repeat_ix) {
      dataset <- datasets[[dataset_id]]
      expr <- get_expression(dataset)

      time0 <- Sys.time()
      importances <- dynfeature::calculate_overall_feature_importance(traj = dataset, expression_source = expr)
      time1 <- Sys.time()
      execution_time <- difftime(time1, time0, units = "secs") %>% as.numeric()

      tibble(
        repeat_ix,
        id = dataset$id,
        nrow = nrow(expr),
        ncol = ncol(expr),
        importances = list(importances),
        execution_time
      )
    })

  write_rds(lst(dataset_ids, dataset_metadata, num_repeats), result_file("fimp_orig.rds"), compress = "xz")
}

list2env(read_rds(result_file("fimp_orig.rds")), .GlobalEnv)
datasets <- load_datasets(ids = dataset_ids, as_tibble = FALSE) %>% set_names(dataset_ids)

###############################################################
###                 DETERMINE ORIGINAL IMPS                 ###
###############################################################
pairwise_cor_fun <- function(metadata1, metadata2, same = TRUE) {
  cr <- crossing(
    id = dataset_ids,
    left = unique(metadata1$repeat_ix),
    right = unique(metadata2$repeat_ix)
  )

  if (!same) {
    cr <- cr %>% filter(left != right)
  }

  cr %>%
    left_join(metadata1 %>% select(left = repeat_ix, id, importances_left = importances), by = c("id", "left")) %>%
    left_join(metadata2 %>% select(right = repeat_ix, id, importances_right = importances), by = c("id", "right")) %>%
    rowwise() %>%
    mutate(cor = dyneval:::.calculate_featureimp_cor(importances_left, importances_right)$featureimp_wcor) %>%
    ungroup() %>%
    select(-importances_left, -importances_right)
}

pairwise_cor_orig_orig <- pairwise_cor_fun(dataset_metadata, dataset_metadata, same = FALSE)

pairwise_cor_dist <-
  pairwise_cor_orig_orig %>%
  group_by(id) %>%
  summarise(mean = mean(cor), sd = sd(cor))

g1 <-
  ggplot(pairwise_cor_orig_orig) +
  geom_density(aes(cor, colour = id), size = 1) +
  theme_bw() +
  scale_colour_brewer(palette = "Set3") +
  labs(title = "Pairwise cor of importance scores, orig vs. orig")
g1
ggsave(result_file("pairwise_cor_orig_orig.pdf"), g1, width = 10, height = 6)

g2 <-
  crossing(pairwise_cor_dist, data_frame(cor = seq(min(pairwise_cor_orig_orig$cor), 1, by = .001))) %>%
  mutate(dens = dnorm(cor, mean, sd)) %>%
  ggplot() +
  geom_line(aes(cor, dens, colour = id), size = 1) +
  theme_bw() +
  scale_colour_brewer(palette = "Set3") +
  labs(title = "Estimated densities of pairwise cors")
g2
ggsave(result_file("pairwise_cor_orig_orig_dens.pdf"), g2, width = 10, height = 6)


###############################################################
###                       PARAM OPTIM                       ###
###############################################################

# you need to load mlrMBO, otherwise some things might not work...
library(mlrMBO) # install.packages(c("DiceKriging", "rgenoud"))

num_cores <- 1
num_iters <- 200

# resume from previous results, if any exist
design <-
  if (!file.exists(result_file("opt_path.rds"))) {
    crossing(
      num_trees = c(500, 1000, 2000),
      num_mtry = c(20, 50, 100),
      num_sample = c(50, 100, 200),
      min_node_size = c(1, 5, 10)
    ) %>%
      sample_n(30)
  } else {
    read_rds(result_file("opt_path.rds"))
  }

# toggle this to perform mlrmbo optimisation
im_lazy_and_just_want_to_run_the_plots_below <-
  TRUE && "score" %in% colnames(design)

if (im_lazy_and_just_want_to_run_the_plots_below) {
  opt_path <- design
} else {

  # determine params to optimise over
  param_set <- ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam(id = "num_trees", lower = log10(100), upper = log10(10000), trafo = function(x) round(10 ^ x)),
    ParamHelpers::makeNumericParam(id = "num_mtry", lower = log10(5), upper = log10(100), trafo = function(x) round(10 ^ x)),
    ParamHelpers::makeNumericParam(id = "num_sample", lower = log10(10), upper = log10(1000), trafo = function(x) round(10 ^ x)),
    ParamHelpers::makeIntegerParam(id = "min_node_size", lower = 1L, upper = 20L)
  )

  # determine function for turning the execution time into a score
  # 1800 seconds: score = 0
  # 0 seconds: score = 1
  scale_execution_time <- function(x) {
    1 - pmin(x / 1800, 1)
  }

  # define an objective function for the optimisation
  obj_fun <-
    smoof::makeSingleObjectiveFunction(
      name = "TItrain",
      vectorized = FALSE,
      minimize = FALSE,
      has.simple.signature = FALSE,
      par.set = param_set,
      fn = function(x) {
        num_repeats <- 1

        # run a fimp on each of the datasets with the given parameters
        new_metadata <-
          crossing(
            dataset_id = dataset_ids,
            repeat_ix = seq_len(num_repeats)
          ) %>%
          pmap_df(function(dataset_id, repeat_ix) {
            dataset <- datasets[[dataset_id]]
            expr <- get_expression(dataset)

            # transform parameters
            method_params <- list(
              num.trees = x$num_trees,
              mtry = min(x$num_mtry, ncol(expr)),
              sample.fraction = min(x$num_sample / nrow(expr), 1),
              min.node.size = x$min_node_size,
              splitrule = x$splitrule,
              write.forest = FALSE
            )

            # run fimp
            time0 <- Sys.time()
            importances <- dynfeature::calculate_overall_feature_importance(
              traj = dataset,
              expression_source = expr,
              method_params = method_params
            )
            time1 <- Sys.time()

            # calculate execution time
            execution_time <- difftime(time1, time0, units = "secs") %>% as.numeric()

            tibble(
              repeat_ix,
              id = dataset$id,
              importances = list(importances),
              execution_time
            )
          })

        # calculate the sum of times (averages across replicates, if any)
        execution_time <-
          new_metadata %>%
          group_by(id) %>%
          summarise(mean = mean(execution_time)) %>%
          summarise(sum = sum(mean)) %>%
          pull(sum)

        # determine how well the fimps correlate with the original datasets
        pnorm_cor <-
          pairwise_cor_fun(dataset_metadata, new_metadata) %>%
          mutate(cor = ifelse(is.finite(cor), cor, 0)) %>%
          left_join(pairwise_cor_dist, by = "id") %>%
          mutate(pnorm_cor = pnorm(cor, mean, sd)) %>%
          summarise(mean = mean(pnorm_cor)) %>%
          pull(mean)

        # create a summary table
        summary <-
          as_data_frame(x) %>%
          mutate(
            pnorm_cor,
            execution_time,
            execution_time_score = scale_execution_time(execution_time),
            score = dyneval::calculate_geometric_mean(pnorm_cor, execution_time_score)
          )

        score <- summary$score

        attr(score, "extras") <- list(
          .summary = summary
        )

        score
      }
    )

  # determine some mlrmbo settings
  progress_file <- derived_file("mlr_progress.RData")

  if (file.exists(progress_file)) file.remove(progress_file)
  control <-
    mlrMBO::makeMBOControl(
      n.objectives = 1,
      propose.points = num_cores,
      save.file.path = progress_file,
      save.on.disk.at = seq(0, num_iters + 1, by = 1),
      y.name = "score"
    ) %>%
    mlrMBO::setMBOControlTermination(iters = num_iters)

  if (length(control$y.name) == 1) {
    control <- control %>%
      mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritCB())
  } else {
    control <- control %>%
      mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritDIB())
  }

  # run mlrmbo
  mbo_out <-
    mlrMBO::mbo(
      fun = obj_fun,
      control = control,
      design = design %>%
        select(one_of(c(names(param_set$pars), "score"))) %>% # only pass the parameter columns and the score column, if any exists
        mutate_at(c("num_trees", "num_mtry", "num_sample"), log10),
      show.info = TRUE
    )

  # if mlrmbo errored or was cancelled, you can finalise the results as follows:
  mbo_out <- mboFinalize(progress_file)

  opt_path <-
    bind_rows(
      { if ("score" %in% colnames(design)) design else NULL },
      mbo_out$opt.path$env$extra %>%
        map_df(~ .$.summary)
    )

  write_rds(opt_path, result_file("opt_path.rds"), compress = "xz")
}


g1 <- ggplot(opt_path) + geom_point(aes(pnorm_cor, execution_time, colour = score), size = 3) + theme_bw() + viridis::scale_colour_viridis()
ggsave(result_file("result_cor_vs_time.pdf"), g1, width = 6, height = 5)

g2 <- ggplot(opt_path %>% gather(parameter, value, num_trees:min_node_size, execution_time)) +
  geom_point(aes(value, pnorm_cor, colour = score), size = 3) +
  theme_bw() +
  viridis::scale_colour_viridis() +
  facet_wrap(~parameter, scales = "free")
ggsave(result_file("result_param_vs_cor.pdf"), g2, width = 12, height = 8)

opt_path %>% arrange(desc(pnorm_cor))
opt_path %>% arrange(desc(score))










# ###############################################################
# ###                   DETERMINE LITE IMPS                   ###
# ###############################################################
# params <- lst(
#   num_trees = 1000,
#   num_mtry = 20,
#   num_sample = 100
# )
#
# new_metadata <-
#   crossing(
#     dataset_id = dataset_ids,
#     repeat_ix = seq_len(num_repeats)
#   ) %>%
#   pmap_df(function(dataset_id, repeat_ix) {
#     dataset <- datasets[[dataset_id]]
#     expr <- get_expression(dataset)
#
#     method_params <- list(
#       num.trees = params$num_trees,
#       mtry = min(params$num_mtry, ncol(expr)),
#       sample.fraction = min(params$num_sample / nrow(expr), 1)
#     )
#
#     time0 <- Sys.time()
#     importances <- dynfeature::calculate_overall_feature_importance(
#       traj = dataset,
#       expression_source = expr,
#       method_params = method_params
#     )
#     time1 <- Sys.time()
#     execution_time <- difftime(time1, time0, units = "secs") %>% as.numeric()
#
#     tibble(
#       repeat_ix,
#       id = dataset$id,
#       importances = list(importances),
#       execution_time
#     )
#   })
#
# pairwise_cor_orig_new <- pairwise_cor_fun(dataset_metadata, new_metadata)
# pairwise_cor_new_new <- pairwise_cor_fun(new_metadata, new_metadata, same = FALSE)
#
# pairwise_cor <-
#   bind_rows(
#     pairwise_cor_orig_orig %>% mutate(comparison = "orig vs. orig"),
#     pairwise_cor_orig_new %>% mutate(comparison = "orig vs. new"),
#     pairwise_cor_new_new %>% mutate(comparison = "new vs. new")
#   )
# ggplot(pairwise_cor) + geom_density(aes(cor, colour = id), size = 1) + theme_bw() +
#   scale_colour_brewer(palette = "Set3") + labs(title = "Pairwise cor of importance scores, orig vs. new") + facet_wrap(~comparison, ncol = 1)
#
# execution_times <-
#   bind_rows(
#     dataset_metadata %>% mutate(comparison = "orig"),
#     new_metadata %>% mutate(comparison = "new")
#   ) %>%
#   group_by(id, comparison) %>%
#   summarise(execution_time = mean(execution_time)) %>%
#   ungroup() %>%
#   spread(comparison, execution_time)
#
# sum(dataset_metadata$execution_time)
# sum(new_metadata$execution_time)
#
# ggplot(execution_times) + geom_point(aes(new, orig)) + theme_bw()
