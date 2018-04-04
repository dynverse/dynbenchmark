library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

# read param eval config
eval_param_config <- read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters"))

# read param eval results
eval_param_outputs <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))

# read tasks
tasks <- map_df(
  paste0(eval_param_config$local_tasks_folder, "/", eval_param_config$task_ids, ".rds"),
  function(file_path) {
    read_rds(file_path) %>% mutate(ncell = nrow(expression[[1]]), nfeat = ncol(expression[[1]]))
  }
)

# # read complexities
# complexities <- read_rds(derived_file("dimensionality_versus_timings_data.rds", "5-optimise_parameters/4-plots"))
#
# # read variances
# variances <- read_rds(derived_file("variance_results.rds", "5-optimise_parameters/4-plots"))

# read method meta info
meta <- read_rds(derived_file("methods_evaluated.rds", "4-method_characterisation")) %>%
  mutate(
    prior_mini_id = paste0("prior_mini_", group_indices(group_by_at(., vars(!!setdiff(priors$prior_id, "time")))))
  )

# read qc scores
qc_category_scores <- read_rds(derived_file("implementation_qc_category_scores.rds", "4-method_characterisation"))
qc_application_scores <- read_rds(derived_file("implementation_qc_application_scores.rds", "4-method_characterisation"))


## START GATHERING COLUMNS
part_overall_mean <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type == "overall", task_source == "mean") %>%
  select(
    method_short_name, harm_mean, norm_correlation, norm_edge_flip, norm_rf_mse, time_method, rank_time_method,
    num_files_created, num_setseed_calls, pct_errored, pct_time_exceeded, pct_memory_exceeded, pct_allerrored, pct_stochastic
  ) %>%
  mutate(overall_metric = apply(.[,colnames(.)[grepl("^norm_", colnames(.))]], 1, mean))

part_sources <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type == "overall", task_source != "mean") %>%
  select(method_short_name, task_source, harm_mean) %>%
  mutate(task_source = paste0("source_", task_source)) %>%
  spread(task_source, harm_mean) %>%
  mutate(overall_source = apply(.[,colnames(.)[grepl("^source_", colnames(.))]], 1, mean))

part_trajtypes <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type != "overall", task_source == "mean") %>%
  select(method_short_name, trajectory_type, harm_mean) %>%
  mutate(trajectory_type = paste0("trajtype_", trajectory_type)) %>%
  spread(trajectory_type, harm_mean) %>%
  mutate(overall_trajtype = apply(.[,colnames(.)[grepl("^trajtype_", colnames(.))]], 1, mean))

# part_complexities <-
#   complexities$complexity %>%
#   select(
#     method_short_name, complexity_inferred = feature, complexity_inferred2 = feature2, complexity_sign = sign, complexity_rsq = rsq
#   )
#
# part_variances <-
#   variances$vardf %>%
#   select(
#     method_short_name, mean_var, var_norm_correlation, var_norm_edge_flip, var_norm_rf_mse, sets_seeds
#   )

part_priors <-
  eval_param_outputs$outputs_ind %>%
  select(method_short_name, prior_str) %>%
  distinct()

part_method_characterisation <-
  meta %>%
  select(
    method_short_name = method_id,
    method_name,
    implementation_id,
    implementation_name,
    output_transformation,
    topology_inference_type,
    maximal_trajectory_types,
    earliest_date = date,
    publication_date,
    preprint_date,
    ncitations,
    qc_score,
    starts_with("prior_")
  )

part_qc_category <-
  qc_category_scores %>%
  mutate(category = paste0("qc_cat_", category)) %>%
  spread(category, qc_score) %>%
  right_join(meta %>% select(implementation_id, method_id), by = "implementation_id") %>%
  select(method_short_name = method_id, everything()) %>%
  select(-implementation_id) %>%
  mutate(overall_qccat = apply(.[,colnames(.)[grepl("^qc_cat_", colnames(.))]], 1, mean))

part_qc_application <-
  qc_application_scores %>%
  mutate(application = paste0("qc_app_", application)) %>%
  spread(application, score) %>%
  right_join(meta %>% select(implementation_id, method_id), by = "implementation_id") %>%
  select(method_short_name = method_id, everything()) %>%
  select(-implementation_id) %>%
  mutate(overall_qcapp = apply(.[,colnames(.)[grepl("^qc_app_", colnames(.))]], 1, mean))

## COMBINE_COLUMNS
part_list <- lst(
  part_overall_mean,
  part_sources,
  part_trajtypes,
  # part_complexities,
  # part_variances,
  part_priors,
  part_method_characterisation,
  part_qc_category,
  part_qc_application
)
reduce_fun <- function(a, b) inner_join(a, b, by = "method_short_name")
method_tib <- Reduce("reduce_fun", part_list) %>%
  mutate(
    overall_benchmark = apply(cbind(overall_metric, overall_source, overall_trajtype), 1, psych::harmonic.mean),
    is_na_qc = is.na(overall_qccat),
    overall_qccat = ifelse(is_na_qc, mean(overall_qccat, na.rm = T), overall_qccat),
    overall_qcapp = ifelse(is_na_qc, mean(overall_qcapp, na.rm = T), overall_qcapp),
    overall_qc = apply(cbind(overall_qccat, overall_qcapp), 1, psych::harmonic.mean),
    overall_qc = ifelse(is_na_qc, 0, overall_qc),
    overall = apply(cbind(overall_metric, overall_source, overall_trajtype, overall_qccat, overall_qcapp), 1, psych::harmonic.mean)
  ) %>%
  filter(!method_short_name %in% c("gng", "periodpc"))


## CALCULATE FINAL RANKING
method_name_ord <- method_tib %>% arrange(desc(overall)) %>% pull(method_name)
method_tib <- method_tib %>% mutate(method_name_f = factor(method_name, levels = method_name_ord))

# method name check
method_short_names <- part_list %>% map(~.$method_short_name) %>% Reduce("union", .)
for (n in names(part_list)) {
  dff <- setdiff(method_short_names, part_list[[n]]$method_short_name)
  if (length(dff) > 0) {
    cat(n, " missing methods: ", paste(dff, collapse = ", "), "\n", sep = "")
  }
}

# construct minis
trajectory_types_mini <- tibble(
  id = list.files(figure_file("", "trajectory_types/mini")) %>% str_replace(".svg$", ""),
  svg = id %>% map(~as.character(xml2::read_xml(figure_file(paste0(., ".svg"), "trajectory_types/mini"))))
)

minis <- trajectory_types_mini %>% create_replacers()

# write output
write_rds(lst(method_tib, minis), result_file("aggregated_data.rds"))

method_tib$method_short_name %>% sort
