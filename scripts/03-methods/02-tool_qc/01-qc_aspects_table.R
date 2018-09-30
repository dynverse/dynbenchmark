#' Generate a table containing the qc scoresheet

library(dynbenchmark)
library(tidyverse)
library(xlsx)

experiment("03-methods/02-tool_qc")

qc_checks <- readRDS(result_file("qc_checks.rds", "03-methods"))

qc_checks %>%
  as.data.frame() %>%
  write.xlsx(result_file("qc_scoresheet.xlsx"), row.names = FALSE)
