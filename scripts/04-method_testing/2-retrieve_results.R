#' Retrieve the results

library(dynbenchmark)
library(tidyverse)

experiment("04-method_testing")

###################################################
###                    FETCH                    ###
###################################################
benchmark_fetch_results()

output <- benchmark_bind_results(load_models = TRUE)

design <- read_rds(derived_file("design.rds"))

extract_method_status <- function(error_status, correlation, ...) {
  case_when(
    error_status != "no_error" ~ error_status,
    correlation < 0.5 ~ "low_correlation",
    TRUE ~ "no_error"
  )
}
output$method_status <- pmap_chr(output, extract_method_status)

write_rds(output, result_file("output.rds"), compress = "xz")

checks <-
  output %>%
  mutate(time_method = ifelse(is.na(time_method), 1200, time_method)) %>%
  group_by(method_id) %>%
  summarise(
    ran = mean(!method_status %in% c("method_error", "execution_error")),
    time = sum(time_method)
  ) %>%
  arrange(ran < 0, time)

write_rds(checks, result_file("checks.rds"), compress = "xz")
