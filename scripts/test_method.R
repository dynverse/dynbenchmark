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

bigtasks <- read_rds("~/bigtasks.rds")

## chose method
method <- description_sincell()

par_set <- method$par_set
default_params <- ParamHelpers::generateDesignOfDefaults(par_set) %>% ParamHelpers::dfRowToList(par_set, 1)
list2env(default_params, environment())

# laad task in environment
bigtask <- dynutils::extract_row_to_list(bigtasks, nrow(bigtasks))
smalltask <- extract_row_to_list(dyntoy::toy_tasks, 1)

task_to_evaluate <- smalltask
list2env(task_to_evaluate, environment())
list2env(prior_information, environment())




tasks_to_evaluate <- dynutils::list_as_tibble(list(task_to_evaluate))
tasks_to_evaluate <- bigtasks


##
results <- map(seq(18, nrow(tasks_to_evaluate)), function(row_id) {
  print(row_id)
  bigtasks <- tasks_to_evaluate[row_id, ,drop=F]
  execute_evaluation(bigtasks, method, default_params, "correlation", extra_metrics=c("rf_mse", "edge_flip")) %>% attr("extras") %>% .$.summary
}) %>% bind_rows()



results$correlation




result <- dynmethods:::run_sincell(task_to_evaluate$expression)
result$geodesic_distances <- dynutils::compute_emlike_dist(result)

dyneval:::calculate_metrics(task_to_evaluate, result, "rf_mse")
