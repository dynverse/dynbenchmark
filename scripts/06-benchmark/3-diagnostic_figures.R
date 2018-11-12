#' Create some diagnostic figures to get an overview of the results

library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("06-benchmark")


############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

num_replicates <- 1

list2env(read_rds(result_file("benchmark_results_input.rds")), environment())
list2env(read_rds(result_file("benchmark_results_normalised.rds")), environment())

metrics_info <- dyneval::metrics %>%
  slice(match(metrics, metric_id)) %>%
  add_row(metric_id = "overall", plotmath = "overall", latex = "\\textrm{overall}", html = "overall", long_name = "Overall score", category = "average", type = "overall", perfect = 1, worst = 0)

# get ordering of methods
method_ord <-
  data_aggregations %>%
  filter(dataset_source == "mean", dataset_trajectory_type == "overall") %>%
  arrange(desc(overall)) %>%
  select(method_id, method_name) %>%
  mutate_all(factor)

# create method_id_f factor in all data structures
data_aggregations <- data_aggregations %>% mutate(method_id = factor(method_id, levels = method_ord$method_id), method_name = factor(method_name, levels = method_ord$method_name))
data <- data %>% mutate(method_id = factor(method_id, levels = method_ord$method_id), method_name = factor(method_name, levels = method_ord$method_name))

# execute plotting scripts
source(scripts_file("3a-overall_comparison.R"))
source(scripts_file("3b-time_mem_predictions.R"))
source(scripts_file("3c-normalisation.R"))
source(scripts_file("3d-compare_sources.R"))
