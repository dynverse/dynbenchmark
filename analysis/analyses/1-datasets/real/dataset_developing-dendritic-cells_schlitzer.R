rm(list=ls())
library(tidyverse)
library(dynalysis)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/developing-dendritic-cells_schlitzer")

# download and untar files
file <- download_dataset_file("GSE60781_RAW.tar", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE60781&format=file")

untar(file, exdir = dataset_preproc_file(""))

# read counts
counts <- map_df(list.files(dataset_preproc_file(""), ".*read[cC]ount"), function(filename) {
  read_tsv(dataset_preproc_file(filename), col_names = c("gene", "count"), col_types = cols(gene = "c", count = "i")) %>%
    mutate(sample = gsub("_.*", "", filename))
}) %>%
  spread(gene, count) %>%
  as.data.frame() %>%
  magrittr::set_rownames(NULL) %>%
  column_to_rownames("sample") %>%
  as.matrix

# download cell info
geo <- GEOquery::getGEO("GSE60781", destdir = dataset_preproc_file(""))
cell_info <- geo[[1]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  mutate(milestone_id = gsub("_[0-9]*", "", title)) %>%
  select(cell_id = geo_accession, milestone_id) %>%
  slice(match(rownames(counts), cell_id))

milestone_network = tribble(
  ~from, ~to,
  "MDP", "CDP",
  "CDP", "PreDC"
) %>% mutate(length = 1, directed = TRUE)

milestone_ids <- c("MDP", "CDP", "PreDC")

cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, group_id = milestone_id)
milestone_percentages <- cell_info %>% select(cell_id, milestone_id) %>% mutate(percentage = 1)

feature_info <- tibble(feature_id = colnames(counts))

datasetpreproc_normalise_filter_wrap_and_save(
  ti_type = "linear",
  counts = counts,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)
