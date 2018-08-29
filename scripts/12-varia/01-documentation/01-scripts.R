#' Read in all the scripts and their documentation

library(dynbenchmark)
library(fs)
library(tidyverse)

experiment("12-varia/01-documentation")

scripts <- extract_scripts_documentation("scripts")

scripts %>% select(id, title)

# save scripts
write_rds(scripts, derived_file("scripts.rds"))
