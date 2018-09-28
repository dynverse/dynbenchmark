library(dynbenchmark)
library(tidyverse)
library(furrr)
plan(multiprocess)

experiment("10-benchmark_interpretation")

# load in output models
output <- benchmark_bind_results(load_models = TRUE, experiment_id = "04-method_testing") %>%
  select(method_id, dataset_id, model)

# only take into account methods with free topology and tree detection
relevant_methods <- load_methods() %>% filter(source != "control", topology_inference == "free", detects_tree) %>% pull(id)
output <- output %>% filter(method_id %in% relevant_methods)


design <- read_rds(derived_file("design.rds", "04-method_testing"))
datasets <- design$datasets %>%
  mutate(dataset = invoke_map(fun)) %>%
  select(dataset_id = id, dataset)

output$trajectory_type_prediction <- output$model %>% map_chr(~ifelse(is.null(.), "unknown", .$trajectory_type))

datasets$trajectory_type_dataset <- datasets$dataset %>% map_chr(~ifelse(is.null(.), "unknown", .$trajectory_type))

models <- output %>%
  select(-model) %>%
  left_join(datasets %>% select(-dataset), "dataset_id")

trajectory_type_crosstab <- models %>%
  group_by(method_id, trajectory_type_prediction, trajectory_type_dataset) %>%
  tally() %>%
  ungroup() %>%
  complete(method_id, trajectory_type_prediction, trajectory_type_dataset, fill = list(n = 0))

trajectory_type_sensitivity <- trajectory_type_crosstab %>%
  group_by(method_id, trajectory_type_dataset) %>%
  summarise(sensitivity = sum(n[trajectory_type_prediction == trajectory_type_dataset])/sum(n)) %>%
  ungroup()

trajectory_type_sensitivity %>%
  filter(method_id %in% relevant_method_ids) %>%
  filter(trajectory_type_dataset == "tree") %>%
  top_n(3, sensitivity)
