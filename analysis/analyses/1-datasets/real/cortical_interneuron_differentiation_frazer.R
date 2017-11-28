rm(list=ls())
library(dynalysis)
library(tidyverse)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/cortical_interneuron_differentiation_frazer")

txt_location <- download_dataset_file(
  "GSE90860_SupplementaryData1_revised_julien.tsv.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE90860&format=file&file=GSE90860%5FSupplementaryData1%5Frevised%5Fjulien%2Etsv%2Egz"
)

counts_df <- read_tsv(txt_location)
counts <- counts_df %>% select(-X1) %>% as.matrix %>% t %>% magrittr::set_colnames(counts_df$X1)

geo <- GEOquery::getGEO(GEO = "GSE90860", destdir = dataset_preproc_file(""))
cell_info <- geo[[1]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  mutate(
    cell_id = rownames(counts),
    milestone_id = paste0(gsub("age: (.*)", "\\1", characteristics_ch1), "#", gsub("assigned_subgroup: (.*)", "\\1", characteristics_ch1.1))
  ) %>%
  select(cell_id, milestone_id) %>%
  mutate_all(funs(as.character))

milestone_network <- tribble(
  ~from, ~to, ~length,
  "E18#0", "P2#1", 4,
  "E18#0", "P2#2", 4,
  "E18#0", "P2#3", 4,
  "P2#1", "P5#1", 3,
  "P2#2", "P5#2", 3,
  "P2#3", "P5#3", 3
) %>% mutate(directed = TRUE)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

feature_info <- tibble(feature_id = colnames(counts))

datasetpreproc_normalise_filter_wrap_and_save(
  ti_type = "trifurcating",
  counts = counts,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)
