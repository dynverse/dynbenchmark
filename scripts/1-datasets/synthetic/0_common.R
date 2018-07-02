library(tidyverse)
library(PRISM)
library(dynverse)

dataset_id <- "synthetic/v7"
dataset_preprocessing(dataset_id)
remote_folder <- "/group/irc/shared/dynbenchmark/" # dynbenchmark on prism
