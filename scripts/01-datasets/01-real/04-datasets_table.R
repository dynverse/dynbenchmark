#' Creates a table of the datasets in, excuse me, excel (for supplementary material)

library(dynbenchmark)
library(tidyverse)
library(xlsx)

experiment("01-datasets/01-real")

metadata <- read_rds(result_file("metadata.rds"))

metadata %>%
  mutate(notes = ifelse(is.na(notes), "", notes)) %>%
  as.data.frame() %>%
  write.xlsx(result_file("real_datasets.xlsx"), row.names = FALSE, showNA = FALSE)
