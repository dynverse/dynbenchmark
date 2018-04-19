library(tidyverse)
library(dynalysis)

dataset_preprocessing("real/macrophage-salmonella_saliba")

txt_location <- download_dataset_file(
  "GSE79363_first_dataset_read_count.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE79363&format=file&file=GSE79363%5Ffirst%5Fdataset%5Fread%5Fcount%2Etxt%2Egz"
)

counts <- read_tsv(txt_location) %>% as.data.frame() %>% tibble::column_to_rownames("X1") %>% as.matrix() %>% t
cell_info <- tibble(cell_id = rownames(counts), milestone_id = gsub(".*_([A-Za-z]*)$", "\\1", rownames(counts)))

counts <- counts[cell_info$cell_id, ]

milestone_network = tribble(
  ~from, ~to,
  "NNI", "MNGB",
  "NNI", "MGB",
  "NNI", "bystanders"
) %>% mutate(length = 1, directed = TRUE)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

feature_info <- tibble(feature_id = colnames(counts))

preprocess_dataset(
  counts = counts,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)
