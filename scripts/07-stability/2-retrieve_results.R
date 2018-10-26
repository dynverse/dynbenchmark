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



# # bind results in one data frame (without models)
# execution_output <-
#   benchmark_bind_results(
#     load_models = FALSE,
#     filter_fun = function(df) {
#       df %>% filter(!method_id %in% c("identity", "shuffle", "error", "random"))
#     }
#   )
#
# table(execution_output$method_id, execution_output$error_status)
#
# ##############################################################
# ###                        JOIN DATA                       ###
# ##############################################################
#
# datasets <- read_rds(derived_file("datasets.rds", "07-stability"))
#
# raw_data <-
#   execution_output %>%
#   rename(did_bs = dataset_id) %>%
#   left_join(datasets %>% select(did_bs = id, dataset_id = orig_dataset_id), by = "did_bs")
#
# dataset_ids <- unique(raw_data$dataset_id)
#
# orig_datasets <- load_datasets(ids = dataset_ids)
#
# raw_data <- raw_data %>%
#   left_join(orig_datasets %>% select(dataset_id = id, dataset_trajectory_type = trajectory_type, dataset_source = source), by = "dataset_id")
# #
# # write_rds(raw_data, derived_file("benchmark_results_unnormalised.rds"))
# method_ids <- unique(raw_data$method_id)



##############################################################
###             SUBMIT PAIRWISE COMPARISON JOBS            ###
##############################################################
metric_ids <- c("correlation", "him", "featureimp_wcor", "F1_milestones")

pairwise_submit <- function() {
  requireNamespace("qsub")

  # load stability dataset info
  datasets <- read_rds(derived_file("datasets.rds", "07-stability"))
  orig_dataset_ids <- unique(datasets$orig_dataset_id)

  # determine output folders
  local_output_folder <- derived_file("suite")
  remote_output_folder <- derived_file("suite", remote = TRUE)

  # find all finished jobs
  metric_files <- list.files(local_output_folder, pattern = "output_metrics.rds", recursive = TRUE, full.names = TRUE)

  # make sure the remote is rsynced with the host, otherwise this job will fail
  walk(metric_files, function(metric_file) {
    folder <- gsub("/output_metrics\\.rds$", "", metric_file)
    name <- gsub(paste0(local_output_folder, "/"), "", folder, fixed = TRUE) %>% gsub("/", "_", .)
    rem_folder <- gsub(local_output_folder, remote_output_folder, folder, fixed = TRUE)

    pairwise_file <- paste0(folder, "/output_pairwise.rds")
    qsubhandle_file <- paste0(folder, "/qsubhandle_pairwise.rds")

    if (file.exists(qsubhandle_file)) {
      cat(name, ": Job still running\n", sep = "")
    }
    if (file.exists(pairwise_file)) {
      cat(name, ": Already calculated\n", sep = "")
    }

    if (!file.exists(pairwise_file) && !file.exists(qsubhandle_file)) {
      tmp_path <- paste0(folder, "/r2gridengine_pairwise")
      tmp_path_rem <- gsub(local_output_folder, remote_output_folder, tmp_path, fixed = TRUE)

      cat(name, ": Submitting job\n", sep = "")

      qsub_config <- qsub::override_qsub_config(
        name = name,
        memory = "10G",
        max_wall_time = "12:00:00",
        wait = FALSE,
        local_tmp_path = tmp_path,
        remote_tmp_path = tmp_path_rem,
        remove_tmp_folder = FALSE,
        stop_on_error = FALSE
      )

      qsub_fun <- function(orig_dataset_id) {
        experiment("07-stability")

        metric_file <- paste0(rem_folder, "/output_metrics.rds")
        models_file <- paste0(rem_folder, "/output_models.rds")

        # check which datasets were generated from the original dataset
        datasets <-
          read_rds(derived_file("datasets.rds", "07-stability")) %>%
          filter(orig_dataset_id == !!orig_dataset_id)

        # load metrics and models
        metrics <- read_rds(metric_file)
        models <- read_rds(models_file)

        # fetch method_id
        method_id <- metrics$method_id[[1]]

        # retain only relevant metrics and models
        ix <- which(metrics$dataset_id %in% datasets$id)
        metrics <- metrics %>% slice(ix)
        models <- models[ix]

        # load original dataset
        orig_dataset <- load_datasets(ids = orig_dataset_id) %>% extract_row_to_list(1)

        crossing(
          i = seq_along(models),
          j = seq_along(models)
        ) %>%
          # filter(i != j) %>% # runs longer
          filter(i + 1 == j) %>% # compare less models, for now
          pmap_dfr(function(i, j) {
            did_bs_i <- metrics$dataset_id[[i]]
            did_bs_j <- metrics$dataset_id[[j]]

            modeli <- models[[i]]
            modelj <- models[[j]]

            eval_out <- tryCatch({
              if (is.null(modeli)) stop("model i is null")
              if (is.null(modelj)) stop("model j is null")

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
              dyneval::calculate_metrics(modeli, modelj, expression_source = expr, metrics = metric_ids)
            }, error = function(e) {
              dyneval::metrics %>% select(metric_id, worst) %>% filter(metric_id %in% !!metric_ids) %>% deframe() %>% t() %>% as_data_frame()
            })

            # calculate geom mean
            eval_out$geom_mean <-
              eval_out %>%
              select(one_of(metric_ids)) %>%
              dyneval::calculate_geometric_mean()

            # add information and reorder columns
            eval_out %>%
              mutate(dataset_id = orig_dataset_id, method_id, indexi = i, indexj = j) %>%
              select(method_id, dataset_id, indexi, indexj, everything())
          })

      }

      qsub_handle <-
        qsub::qsub_lapply(
          X = orig_dataset_ids,
          qsub_config = qsub_config,
          qsub_environment = c("rem_folder", "metric_ids"),
          qsub_packages = c("dynbenchmark", "tidyverse"),
          FUN = qsub_fun
        )
      write_rds(qsub_handle, qsubhandle_file)
    }
  })

  invisible()
}

pairwise_submit()

##############################################################
###                 FETCH PAIRWISE RESULTS                 ###
##############################################################
pairwise_fetch_results <- function(remote = NULL) {
  requireNamespace("qsub")

  local_output_folder <- derived_file("suite")

  # find all 2nd level folders with individual tasks
  handles <- list.files(local_output_folder, pattern = "qsubhandle_pairwise.rds", recursive = TRUE, full.names = TRUE)

  # check for running job ids
  running_job_ids <-
    if (!is.null(remote)) {
      qsub::qstat_remote(remote = remote) %>%
        gsub("^ *([0-9]*).*", "\\1", .) %>%
        unique() %>%
        keep(~ . != "")
    } else {
      c()
    }

  # process each method separately
  map(handles, function(handle) {
    name <- handle %>% gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>% gsub("/qsubhandle_pairwise.rds", "", ., fixed = TRUE)
    output_pairwise_file <- gsub("qsubhandle_pairwise.rds", "output_pairwise.rds", handle, fixed = TRUE)

    if (!file.exists(handle)) {
      cat(name, ": No qsub file was found.\n", sep = "")
      return(FALSE)
    }

    if (file.exists(output_pairwise_file)) {
      cat(name, ": Output already present.\n", sep = "")
      return(FALSE)
    }

    cat(name, ": Attempting to retrieve output from cluster. ", sep = "")
    qsub_handle <- readr::read_rds(handle)

    if (qsub_handle$job_id %in% running_job_ids) {
      cat("Job is still running.\n")
      return(FALSE)
    }

    # attempt to retrieve results; return NULL if job is still busy or has failed
    output <- qsub::qsub_retrieve(
      qsub_handle,
      wait = FALSE
    )

    if (is.null(output)) {
      qstat_out <-
        tryCatch({
          qsub::qstat_j(qsub_handle)
        }, error = function(e) {
          NULL
        }, warning = function(w) {})
      if (is.null(qstat_out)) {
        cat("The job had finished but no output was found.\n")
      } else {
        cat("The job is still running.\n")
      }

      return(FALSE)
    }

    outputs <- bind_rows(output)

    # save output
    readr::write_rds(outputs, output_pairwise_file)

    return(TRUE)
  })

  # return nothing
  invisible()
}


pairwise_fetch_results(remote = TRUE)



##############################################################
###                 FETCH PAIRWISE RESULTS                 ###
##############################################################

pairwise_bind_results <- function() {
  handles <- list.files(derived_file("suite"), pattern = "output_pairwise.rds", recursive = TRUE, full.names = TRUE)
  map_dfr(handles, readr::read_rds)
}

pairwise_bind_results()












#
# out <- qsub::qsub_retrieve(qsub_handle)
#
# pairwise_evals <- bind_rows(out)
#
#
# df %>% select(method_id, dataset_id, one_of(c("geom_mean", metric_ids))) %>% gather(metric, value, -method_id, -dataset_id) %>%
#   ggplot() +
#   geom_histogram(aes(value, fill = metric), binwidth = .05) +
#   facet_wrap(method_id~metric, ncol = length(metrics) + 1, scales = "free_y") +
#   theme_bw() +
#   scale_fill_brewer(palette = "Dark2")
#
#
# df %>% group_by(method_id) %>% summarise_at(c("geom_mean", metrics), mean)



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

