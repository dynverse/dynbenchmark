rm(list=ls())
library(dynalysis)
library(tidyverse)

id <- "distal_lung_epithelium_treutlein"
dataset_preprocessing("real", id)

txt_web_location <- "http://www.nature.com/nature/journal/v509/n7500/extref/nature13173-s4.txt"
txt_location <- dataset_preproc_file("nature13173-s4.txt")

if (!file.exists(txt_location)) {
  download.file(txt_web_location, txt_location, method = "libcurl")
}

df <- read_tsv(txt_location, col_types = cols(cell_name = "c", time_point = "c", sample = "c", putative_cell_type = "c", .default = "d"))
expression <- df[, -c(1:4)] %>% as.matrix() %>% magrittr::set_rownames(expr$cell_name)
cell_info <- df[, c(1:4)] %>% as.data.frame() %>% magrittr::set_rownames(expr$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = putative_cell_type
  ) %>% filter(sample == "singleCell")
expression <- expression[cell_info$cell_id, ]

milestone_network = tribble(
  ~from, ~to,
  "BP", "AT1",
  "BP", "AT2"
) %>% mutate(length = 1, directed = TRUE)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
expression <- expression[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

feature_info <- tibble(feature_id = colnames(expression))

# TODO: check whether the original counts exist
counts <- round(2^expression - 1)

dataset <- wrap_ti_task_data(
  ti_type = "real",
  id = id,
  counts = counts,
  expression = expression,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)

save_dataset(dataset)
