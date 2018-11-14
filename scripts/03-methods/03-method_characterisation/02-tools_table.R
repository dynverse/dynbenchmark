#' Generate a table containing the methods

library(googlesheets)
library(tidyverse)
library(dynbenchmark)
library(xlsx)

experiment("03-methods")

tools <- read_rds(result_file("tools.rds"))

tools <- tools %>%
  arrange(date)

tools %>%
  select(tool_id, tool_name, platform, date, doi) %>%
  as.data.frame() %>%
  write.xlsx(result_file("tools.xlsx"), row.names = FALSE, showNA = FALSE)
