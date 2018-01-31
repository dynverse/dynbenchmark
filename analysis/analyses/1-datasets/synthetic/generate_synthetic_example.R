library(dyngen)
library(tidyverse)
library(dynalysis)

experiment("dyngen_examples")

params <- dyngen::base_params
params$model$modulenet_name <- "bifurcating"
params$simulation$totaltime <- 8
params$simulation$nsimulations <- 50


model <- invoke(generate_model_from_modulenet, params$model)
simulation <- invoke(simulate_multiple, params$simulation, system=model$system)

spaces <- dyngen:::dimred_simulation(simulation, dimred_names = "ica", expression_names = "samplexpression_modules")

plot_simulation_space_time(simulation, spaces) + ggtitle("") + theme(title=element_blank())
