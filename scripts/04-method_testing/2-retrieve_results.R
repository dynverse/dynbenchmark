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

g <- output %>%
  mutate(dataset_id = gsub("specific_example/.*", "specific_example", dataset_id)) %>%
  ggplot(aes(correlation, fct_rev(method_id))) +
  geom_label(aes(label = method_status, fill = method_status)) +
  scale_fill_manual(values = dynbenchmark::method_status_colours) +
  scale_x_continuous(expand = c(0.5, 0)) +
  facet_wrap(~dataset_id, nrow = 1) +
  theme_bw()
g

ggsave(result_file("method_status.svg"), g, width = 16, height = 16)

#' @examples
#' output %>% filter(method_id == "cellrouter") %>% pull(stderr) %>% first() %>% cat
#' output %>% filter(method_id == "stemnet") %>% pull(stdout) %>% first() %>% cat
#'
#' output %>% filter(method_id == "pcreode", grepl("fibro", dataset_id)) %>% pull(stdout) %>% first() %>% cat
#'
#' output %>% filter(method_id == "projected_dpt", grepl("schl", dataset_id)) %>% pull(stderr) %>% first() %>% cat
#'
#' output %>% filter(method_id == "scuba", method_status == "method_error") %>% pull(stdout) %>% cat
#'
#' output %>% filter(str_detect(error_message, "no item called .*")) %>% pull(method_id) %>% unique()
#'
#' output %>% filter(method_id == "urd", method_status == "method_error") %>% select(method_id, dataset_id, stdout, stderr, error_message) %>% pull(stdout)


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
