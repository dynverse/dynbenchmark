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

# read complexities
complexities <- read_rds(derived_file("dimensionality_versus_timings_data.rds", "5-optimise_parameters/4-plots"))

# read variances
variances <- read_rds(derived_file("variance_results.rds", "5-optimise_parameters/4-plots"))

# read method meta info
meta <- read_rds(derived_file("methods_evaluated.rds", "4-method_characterisation"))

# read qc scores
qc_category_scores <- read_rds(derived_file("implementation_qc_category_scores.rds", "4-method_characterisation"))
qc_application_scores <- read_rds(derived_file("implementation_qc_application_scores.rds", "4-method_characterisation"))


## START GATHERING COLUMNS
part1 <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type == "overall", task_source == "mean") %>%
  select(
    method_short_name, method_name, harm_mean, rank_correlation, rank_edge_flip, rank_rf_mse, rank_time_method,
    num_files_created, num_setseed_calls, pct_errored, pct_time_exceeded, pct_memory_exceeded
  )

part1b <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type == "overall", task_source != "mean") %>%
  select(method_short_name, task_source, harm_mean) %>%
  mutate(task_source = paste0("source_", task_source)) %>%
  spread(task_source, harm_mean)

part1c <-
  eval_param_outputs$outputs_summtrajtype_totalsx2 %>%
  filter(trajectory_type != "overall", task_source == "mean") %>%
  select(method_short_name, trajectory_type, harm_mean) %>%
  mutate(trajectory_type = paste0("trajtype_", trajectory_type)) %>%
  spread(trajectory_type, harm_mean)

part2 <-
  complexities$complexity %>%
  select(
    method_short_name, complexity_inferred = feature, complexity_inferred2 = feature2, complexity_sign = sign, complexity_rsq = rsq
  )

part3 <-
  variances$vardf %>%
  select(
    method_short_name, mean_var, var_rank_correlation, var_rank_edge_flip, var_rank_rf_mse, sets_seeds
  )

part4 <-
  eval_param_outputs$outputs_ind %>%
  select(method_short_name, prior_str) %>%
  distinct()

part5 <-
  meta %>%
  select(
    method_short_name = method_id,
    implementation_id,
    implementation_name,
    output_transformation,
    topology_inference_type,
    maximal_trajectory_types,
    earliest_date = date,
    pub_date = PubDate,
    preprint_date = Preprint,
    Citations,
    qc_score
  )

part6 <-
  qc_category_scores %>%
  mutate(category = paste0("qc_", category)) %>%
  spread(category, qc_score) %>%
  right_join(meta %>% select(implementation_id, method_id), by = "implementation_id") %>%
  select(method_short_name = method_id, everything()) %>%
  select(-implementation_id)

## COMBINE_COLUMNS
reduce_fun <- function(a, b) inner_join(a, b, by = "method_short_name")
method_tib <- Reduce("reduce_fun", list(part1, part1b, part1c, part2, part3, part4, part5, part6))

write_rds(method_tib, result_file("method_tib.rds"))
