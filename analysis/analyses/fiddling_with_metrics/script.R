library(dyntoy)
library(dynmethods)
library(dynutils)
library(dynplot)
library(tidyverse)

# install dynalysis
install.packages("devtools")
setRepositories(ind = 1:2)
devtools::install_github("Zouter/dynalysis", dependencies = T, upgrade = T)

# set a seed
set.seed(1)

# generate a dataset. see ?generate_toy_datasets for the different kinds of models available
tasks <- dyntoy::generate_toy_datasets(models = "consecutive_bifurcating", num_replicates = 1, num_cells = 30, num_genes = 100)
dataset <- dynutils::extract_row_to_list(tasks, 1)

# plot the dataset
dynplot::plot_default(dataset)

# choose a method, for instance, slingshot
method <- dynmethods::description_slingshot()

# take the default parameters
default_params <- ParamHelpers::generateDesignOfDefaults(method$par_set, trafo = TRUE) %>% ParamHelpers::dfRowToList(method$par_set, 1)

# execute the method and evaluate it using the correlation of the pairwise geodesic distances
eval <- dyneval::execute_evaluation(
  tasks = tasks,
  method = method,
  parameters = default_params,
  metrics = "correlation",
  timeout = 600
) %>%
  attr("extras")
summary <- eval$.summary
prediction <- eval$.models[[1]]

# look at the summary
summary

# plot the prediction
dynplot::plot_default(prediction)

# these should contain similar elements
names(dataset)
names(prediction)
