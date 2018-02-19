library(dynalysis)
library(tidyverse)
experiment("5-optimise_parameters/13-evaluate_with_real_datasets")

tasks <- read_rds(derived_file("tasks.rds"))

bigtasks <- tasks %>%
  rowwise() %>%
  mutate(
    nrow = nrow(expression),
    ncol = ncol(expression),
    comb = nrow * ncol
  ) %>%
  ungroup() %>%
  arrange(desc(comb)) %>%
  .[, ,drop=F]
bigtask <- dynutils::extract_row_to_list(bigtasks, 1)


bigtask %>% write_rds("~/bigtask.rds")
bigtasks %>% write_rds("~/bigtasks.rds")



#######################"
library(tidyverse)
library(dynalysis)

tasks <- read_rds(derived_file("tasks.rds", experiment_id = "2-dataset_characterisation"))
smalltasks <- dyntoy::toy_tasks

## chose method
description <- description_gpfates()

par_set <- description$par_set
default_params <- ParamHelpers::generateDesignOfDefaults(par_set, trafo = T) %>% ParamHelpers::dfRowToList(par_set, 1)
list2env(default_params, environment())

## laad task in environment
bigtask <- dynutils::extract_row_to_list(tasks, nrow(tasks))
smalltask <- dynutils::extract_row_to_list(tasks,which(tasks$id == "real/cell-cycle_buettner"))
# smalltask <- extract_row_to_list(dyntoy::toy_tasks, 15)# %>% {.$geodesic_dist <- dynutils::compute_emlike_dist(.);.}

task_to_evaluate <- smalltask
list2env(task_to_evaluate, environment())
list2env(prior_information, environment())




tasks_to_evaluate <- dynutils::list_as_tibble(list(task_to_evaluate))
# tasks_to_evaluate <- bigtasks %>% arrange(-row_number())

{
  tasks_to_evaluate$expression <- tasks_to_evaluate$expression %>% map(~.())
  tasks_to_evaluate$counts <- tasks_to_evaluate$counts %>% map(~.())
  tasks_to_evaluate$geodesic_dist <- tasks_to_evaluate$geodesic_dist %>% map(~.())
}


##
results <- execute_method(tasks_to_evaluate, description, default_params, mc_cores=4)

results <- map(seq(nrow(tasks_to_evaluate)), function(row_id) {
  print(paste0(">> ", row_id))
  subtasks <- tasks_to_evaluate[row_id, ,drop=F]
  print(subtasks$id)
  execute_evaluation(subtasks, description, default_params, "correlation", extra_metrics=c("rf_mse", "edge_flip")) %>% attr("extras") %>% .$.summary
}) %>% bind_rows()


results$correlation
results$edge_flip
results$rf_mse




result <- dynmethods:::run_ouijaflow(task_to_evaluate$expression)
result$geodesic_distances <- dynutils::compute_tented_geodesic_distances(result)

result <- wrap_prediction_model_linear(
  cell_ids = rownames(expression),
  pseudotimes = pseudotimes
)
result$geodesic_distances <- dynutils::compute_tented_geodesic_distances(result)

dyneval:::calculate_metrics(task_to_evaluate, result, "correlation")
