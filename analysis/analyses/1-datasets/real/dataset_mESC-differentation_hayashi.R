rm(list=ls())
library(dynalysis)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

id <- "real/mESC-differentiation_hayashi"
dataset_preprocessing(id)

expression_location <- download_dataset_file(
  "GSE98664_tpm_sailfish_mergedGTF_RamDA_mESC_differentiation_time_course.txt.gz",
  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE98nnn/GSE98664/suppl/GSE98664_tpm_sailfish_mergedGTF_RamDA_mESC_differentiation_time_course.txt.gz"
)

counts_all <- read_tsv(expression_location) %>% as.data.frame %>% column_to_rownames("transcript_id") %>% as.matrix() %>% t %>% round()

cell_info_all <- tibble(
  cell_id = rownames(counts_all),
  milestone_id = gsub("RamDA_mESC_(.*)_.*", "\\1", cell_id)
)

milestone_network <- tribble(
  ~from, ~to, ~length,
  "00h", "12h", 1,
  "12h", "24h", 1,
  "24h", "48h", 2,
  "48h", "72h", 2
) %>% mutate(directed = TRUE)


milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- expression_all[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id = group_id) %>% mutate(percentage = 1)

feature_info <- tibble(feature_id = colnames(counts))

datasetpreproc_normalise_filter_wrap_and_save(
  counts = counts,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)
