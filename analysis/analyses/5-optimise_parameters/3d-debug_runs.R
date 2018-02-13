library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

#### FIRST RUN THE RSYNC FROM SCRIPT 3C ####

list2env(read_rds(derived_file("outputs_postprocessed.rds")), environment())
list2env(read_rds(derived_file("config.rds")), environment())

model_short_name <- "scoup"
debug_out <- outputs_ind %>% filter(method_short_name == model_short_name)

i <- 1
model_id <- debug_out$model_id[[i]]
task_id <- debug_out$task_id[[i]]

model <- load_dyneval_model(method_short_name = model_short_name, model_id = model_id)[[1]]
task <- tasks %>% filter(id == task_id) %>% extract_row_to_list(1)

plot_default(task)
plot_default(model)

model$milestone_percentages
