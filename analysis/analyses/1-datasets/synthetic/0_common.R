library(tidyverse)
library(PRISM)
library(dynalysis)

dataset_id <- "synthetic/v7"
dataset_preprocessing(dataset_id)
remote_folder <- "/group/irc/shared/dynalysis/" # dynalysis on prism
