library(cowplot)
library(tidyverse)
library(dynbenchmark)

library(tidygraph)
library(ggraph)

experiment("06-optimise_parameters/10-aggregations")

outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "06-optimise_parameters/3-evaluate_parameters"))
aggregated_data <- read_rds(result_file("aggregated_data.rds", "9-main_figure"))

overall_scores <- aggregated_data$method_tib %>%
  arrange(-overall_benchmark)
method_order <- overall_scores$method_short_name
source_scores <- outputs_list$outputs_summmethod %>%
  inner_join(methods, "method_short_name")
trajtype_scores <- outputs_list$outputs_summtrajtype_totals %>%
  inner_join(methods, "method_short_name")
indrep_scores <- outputs_list$outputs_summrepl %>%
  inner_join(methods, "method_short_name")
ind_scores <- outputs_list$outputs_ind %>%
  inner_join(methods, "method_short_name")

methods <- read_rds(derived_file("methods.rds", experiment_id = "03-method_characterisation")) %>%
  filter(type %in% c("algorithm", "control")) %>%
  filter(method_id %in% overall_scores$method_short_name)

evaluation_algorithm <- lst(overall_scores, method_order, trajtype_scores, indrep_scores, ind_scores, methods)

evaluation_algorithm %>% write_rds(derived_file("evaluation_algorithm.rds"))
