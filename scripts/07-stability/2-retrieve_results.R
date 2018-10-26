library(dynbenchmark)
library(tidyverse)

experiment("07-stability")

##############################################################
###               PART TWO: RETRIEVE RESULTS               ###
##############################################################

# If you are the one who submitted the jobs, run:
benchmark_fetch_results(TRUE)
qsub::rsync_remote(
  remote_src = FALSE,
  path_src = derived_file(remote = FALSE, experiment = "07-stability"),
  remote_dest = TRUE,
  path_dest = derived_file(remote = TRUE, experiment = "07-stability"),
  verbose = TRUE,
  exclude = "*/r2gridengine/*"
)

# If you want to download the output from prism
# qsub::rsync_remote(
#   remote_src = TRUE,
#   path_src = derived_file(remote = TRUE, experiment = "07-stability"),
#   remote_dest = FALSE,
#   path_dest = derived_file(remote = FALSE, experiment = "07-stability"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# bind results in one data frame (without models)
execution_output <-
  benchmark_bind_results(
    load_models = FALSE,
    filter_fun = function(df) {
      df %>% filter(!method_id %in% c("identity", "shuffle", "error", "random"))
    }
  )

table(execution_output$method_id, execution_output$error_status)

##############################################################
###                        JOIN DATA                       ###
##############################################################

datasets <- read_rds(derived_file("datasets.rds", "07-stability"))

raw_data <-
  execution_output %>%
  rename(did_bs = dataset_id) %>%
  left_join(datasets %>% select(did_bs = id, dataset_id = orig_dataset_id), by = "did_bs")

dataset_ids <- unique(raw_data$dataset_id)

orig_datasets <- load_datasets(ids = dataset_ids)

raw_data <- raw_data %>%
  left_join(orig_datasets %>% select(dataset_id = id, dataset_trajectory_type = trajectory_type, dataset_source = source), by = "dataset_id")
#
# write_rds(raw_data, derived_file("benchmark_results_unnormalised.rds"))



##############################################################
###                     PLAY WITH DATA                     ###
##############################################################
metrics <- c("correlation", "him", "featureimp_wcor", "F1_milestones")

grid <-
  raw_data %>%
  select(method_id, dataset_id) %>%
  unique()

qsub_config <- qsub::override_qsub_config(
  name = "dynb07stability",
  memory = "20G",
  max_wall_time = "01:00:00",
  wait = FALSE
)

# make sure the remote is rsynced with the host, otherwise this job will fail
qsub_handle <-
  qsub::qsub_lapply(
    X = seq_len(nrow(grid)),
    qsub_config = qsub_config,
    qsub_environment = c("grid", "metrics"),
    qsub_packages = c("dynbenchmark", "tidyverse"),
    FUN = function(rowi) {
      method_id <- grid$method_id[[rowi]]
      dataset_id <- grid$dataset_id[[rowi]]

      experiment("07-stability")

      datasets <- read_rds(derived_file("datasets.rds", "07-stability")) %>%
        filter(orig_dataset_id == !!dataset_id)

      raw_data <-
        benchmark_bind_results(
          load_models = TRUE,
          filter_fun = function(df) {
            df %>% filter(method_id == !!method_id, dataset_id %in% !!datasets$id)
          }
        ) %>%
        rename(did_bs = dataset_id) %>%
        mutate(dataset_id = !!dataset_id)

      orig_datasets <- load_datasets(ids = dataset_id)

      crossing(
        i = seq_len(nrow(raw_data)),
        j = seq_len(nrow(raw_data))
      ) %>%
        # filter(i != j) %>%
        filter(i + 1 == j) %>% # compare less models, for now
        pmap_dfr(function(i, j) {
          method_id <- raw_data$method_id[[i]]
          dataset_id <- raw_data$dataset_id[[i]]
          did_bs_i <- raw_data$did_bs[[i]]
          did_bs_j <- raw_data$did_bs[[j]]

          modeli <- raw_data$model[[i]]
          modelj <- raw_data$model[[j]]

          eval_out <- tryCatch({
            if (is.null(modeli)) stop("model i is null")
            if (is.null(modelj)) stop("model j is null")
            # if (is.null(modeli) || is.null(modelj)) {
            #   eval_out <- dyneval::metrics %>% select(metric_id, worst) %>% filter(metric_id %in% !!metrics) %>% deframe() %>% t() %>% as_data_frame()
            # }

            orig_dataset <- orig_datasets %>% inner_join(data_frame(id = dataset_id), by = "id") %>% extract_row_to_list(1)
            dataseti <- datasets %>% inner_join(data_frame(id = did_bs_i), by = "id") %>% pull(fun) %>% first() %>% invoke()
            datasetj <- datasets %>% inner_join(data_frame(id = did_bs_j), by = "id") %>% pull(fun) %>% first() %>% invoke()

            # join cell ids and feature ids
            cell_map <-
              full_join(
                dataseti$cell_id_map %>% rename(cell_id_i = cell_id),
                datasetj$cell_id_map %>% rename(cell_id_j = cell_id),
                by = "old_id"
              ) %>% na.omit
            feature_map <-
              full_join(
                dataseti$feature_id_map %>% rename(feature_id_i = cell_id), # change this to feature_id
                datasetj$feature_id_map %>% rename(feature_id_j = cell_id), # change this to feature_id
                by = "old_id"
              ) %>% na.omit

            # revert ids for modeli
            modeli$milestone_percentages <-
              modeli$milestone_percentages %>%
              rename(cell_id_i = cell_id) %>%
              inner_join(cell_map %>% select(cell_id = old_id, cell_id_i), by = "cell_id_i") %>%
              select(-cell_id_i)

            modeli$progressions <-
              modeli$progressions %>%
              rename(cell_id_i = cell_id) %>%
              inner_join(cell_map %>% select(cell_id = old_id, cell_id_i), by = "cell_id_i") %>%
              select(-cell_id_i)

            modeli$cell_ids <- cell_map$old_id

            modeli <- modeli %>% add_cell_waypoints()

            # revert ids for modeli
            modelj$milestone_percentages <-
              modelj$milestone_percentages %>%
              rename(cell_id_j = cell_id) %>%
              inner_join(cell_map %>% select(cell_id = old_id, cell_id_j), by = "cell_id_j") %>%
              select(-cell_id_j)

            modelj$progressions <-
              modelj$progressions %>%
              rename(cell_id_j = cell_id) %>%
              inner_join(cell_map %>% select(cell_id = old_id, cell_id_j), by = "cell_id_j") %>%
              select(-cell_id_j)

            modelj$cell_ids <- cell_map$old_id

            modelj <- modelj %>% add_cell_waypoints()

            # get expression
            expr <- get_expression(orig_dataset)
            expr <- expr[cell_map$old_id, feature_map$old_id]

            # calculate metrics
            # eval_out <- dyneval::calculate_metrics(modeli, modelj, expression_source = expr, metrics = metrics)
            dyneval::calculate_metrics(modeli, modelj, expression_source = expr, metrics = metrics)
          }, error = function(e) {
            dyneval::metrics %>% select(metric_id, worst) %>% filter(metric_id %in% !!metrics) %>% deframe() %>% t() %>% as_data_frame()
          })

          # calculate geom mean
          eval_out$geom_mean <-
            eval_out %>%
            select(one_of(metrics)) %>%
            dyneval::calculate_geometric_mean()

          # add information and reorder columns
          eval_out %>%
            mutate(dataset_id, method_id, indexi = i, indexj = j) %>%
            select(method_id, dataset_id, indexi, indexj, everything())
        })
    }
  )
write_rds(qsub_handle, derived_file("qsub_handle.rds"))

out <- qsub::qsub_retrieve(qsub_handle)

pairwise_evals <- bind_rows(out)


df %>% select(method_id, dataset_id, one_of(c("geom_mean", metrics))) %>% gather(metric, value, -method_id, -dataset_id) %>%
  ggplot() +
  geom_histogram(aes(value, fill = metric), binwidth = .05) +
  facet_wrap(method_id~metric, ncol = length(metrics) + 1, scales = "free_y") +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2")


df %>% group_by(method_id) %>% summarise_at(c("geom_mean", metrics), mean)



# ##############################################################
# ###                   CREATE AGGREGATIONS                  ###
# ##############################################################
#
#
# out <- benchmark_aggregate(
#   data = raw_data %>% mutate(method_name = method_id)
# )
# out$data$overall <- ifelse(is.finite(out$data$overall), out$data$overall, 0)
#
# ##############################################################
# ###                  CALCULATE VARIABILITY                 ###
# ##############################################################
# stability_params <- read_rds(result_file("params.rds", "07-stability"))
#
# worst_var <- var(c(rep(0, floor(stability_params$num_bootstraps / 2)), rep(1, ceiling(stability_params$num_bootstraps / 2))))
#
# norm_var <- function(x) {
#   maxx <- max(x)
#
#   if (maxx == 0) return(1)
#
#   worst_var_this <- worst_var * maxx ^ 2
#
#   (worst_var_this - var(x) ) / worst_var_this
# }
#
# met2 <- c(stability_params$metrics, "overall")
#
# data_stab <-
#   out$data %>%
#   select(method_id, dataset_id, param_id, !!met2) %>%
#   group_by(method_id, dataset_id, param_id) %>%
#   summarise_if(is.numeric, norm_var) %>%
#   ungroup() %>%
#   group_by(method_id, param_id) %>%
#   summarise_if(is.numeric, mean) %>%
#   ungroup() %>%
#   gather(metric, value, !!met2) %>%
#   mutate(metric = paste0("stability_metric_", metric))
#
# data_stab_overall <-
#   data_stab %>%
#   group_by(method_id, param_id) %>%
#   summarise(value = dyneval::calculate_arithmetic_mean(value)) %>%
#   mutate(metric = "stability_overall_overall")
#
# data_stab <- bind_rows(data_stab, data_stab_overall)
#
#
# ##############################################################
# ###                        SAVE DATA                       ###
# ##############################################################
#
# write_rds(data_stab, result_file("stability_results.rds"), compress = "xz")

