rm(list=ls())
library(tidyverse)
library(dynalysis)

dataset_preprocessing("real", "fibroblast_reprogramming_treutlein")

txt_location <- download_dataset_file(
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67310&format=file&file=GSE67310%5FiN%5Fdata%5Flog2FPKM%5Fannotated%2Etxt%2Egz",
  "GSE67310_iN_data_log2FPKM_annotated.txt.gz"
)

df <- read_tsv(txt_location, col_types = cols(cell_name = "c", assignment = "c", experiment = "c", time_point = "c", .default = "d"))
expression <- df[, -c(1:5)] %>% as.matrix() %>% magrittr::set_rownames(df$cell_name)
cell_info <- df[, c(1:5)] %>% as.data.frame() %>% magrittr::set_rownames(df$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = assignment
  )

milestone_network = tribble(
  ~from, ~to,
  "MEF", "d2_intermediate",
  "d2_intermediate", "d2_induced",
  "d2_induced", "d5_intermediate",
  "d5_intermediate", "d5_earlyiN",
  "d5_earlyiN", "Neuron",
  "d5_earlyiN", "Myocyte"
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
  id = datasetpreproc_getid(),
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
