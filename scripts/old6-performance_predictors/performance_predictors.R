library(cowplot)
library(tidyverse)
library(dynbenchmark)

experiment("6-performance_predictors")

required_priors_performance_comparison <- read_rds(figure_file("required_priors_performance_comparison.rds"))


method_components_scores_dots <- read_rds(figure_file("method_components_scores.rds"))
