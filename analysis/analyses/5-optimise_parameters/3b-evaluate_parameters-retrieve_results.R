library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

benchmark_suite_retrieve(derived_file("suite/"), return_outputs = F)

outputs <- benchmark_suite_retrieve(derived_file("suite/"), return_outputs = T) %>% as_tibble()

# print task errors
outputs %>%
  filter(is.na(task_id)) %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n()) %>%
  ungroup()

# print job errors
outputs %>%
  filter(error_message != "") %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n()) %>%
  ungroup()

# process succeeded results
write_rds(outputs, derived_file("outputs_with_models.rds"))
write_rds(outputs %>% select(-model), derived_file("outputs_without_models.rds"))
