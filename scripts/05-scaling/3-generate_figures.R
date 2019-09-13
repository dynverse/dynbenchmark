#' Classify the models and generate scalability figures

library(dynbenchmark)
library(tidyverse)
library(dynutils)

experiment("05-scaling")

method_info <- load_methods()
list2env(read_rds(result_file("scaling.rds")), .GlobalEnv)

models <- models %>% filter(method_id %in% method_info$method_id)

# check which datasets are available on the remote (some may not have executed entirely)
scaling_avail <- qsub::ls_remote(derived_file("", experiment = "05-scaling/dataset", remote = TRUE), remote = TRUE) %>% gsub("\\.rds$", "", .)

scale_x_nrow <- scale_x_continuous(breaks = seq(1, 6), labels = label_thousands(10^seq(1, 6)), expand = c(0,0))
scale_y_ncol <- scale_y_continuous(breaks = seq(1, 6), labels = label_thousands(10^seq(1, 6)), expand = c(0,0))


#' @examples
#' # examine some errors
#' data %>% filter(method_id == "calista", error_status == "method_error", nrow > 100, ncol > 100) %>% select(dataset_id, stdout) %>% pull(stdout)%>% head(5)
#' data %>% filter(method_id == "calista", error_status == "method_error") %>% pull(dataset_id) %>% head(5)
#' data %>% filter(method_id == "calista", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#' data %>% filter(method_id == "dpt", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#'
#' data <- readr::read_rds(dynbenchmark::derived_file("datasets.rds", experiment_id = "05-scaling")) %>% filter(id == "scaling_0001") %>% pull(fun) %>% first() %>% invoke()


##########################################################
###                 RUN FIGURE SCRIPTS                 ###
##########################################################
source(scripts_file("3a-summary_figure.R"))
source(scripts_file("3b-individual_example.R"))
source(scripts_file("3c-individual_overview.R"))
source(scripts_file("3d-error_logs.R"))

##########################################################
###                GENERATE SUPP FIGURE                ###
##########################################################

plot_scaling <- patchwork::wrap_plots(
  read_rds(derived_file("example.rds")) %>% patchwork::wrap_elements(),
  read_rds(derived_file("ranking.rds")) %>% patchwork::wrap_elements(),
  heights = c(2, 6),
  ncol = 1
) +
  patchwork::plot_annotation(tag_levels = "a")

ggsave(result_file("scaling.pdf"), width = 12, height = 16)

