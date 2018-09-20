library(tidyverse)
library(dynbenchmark)
library(GEOquery)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/gold/developing-dendritic-cells_schlitzer")

# download and untar files
file <- download_dataset_source_file("GSE60781_RAW.tar", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE60781&format=file")

untar(file, exdir = dataset_source_file(""))

# read counts
counts <- map_df(list.files(dataset_source_file(""), ".*read[cC]ount"), function(filename) {
  read_tsv(dataset_source_file(filename), col_names = c("gene", "count"), col_types = cols(gene = "c", count = "i")) %>%
    mutate(sample = gsub("_.*", "", filename))
}) %>%
  spread(gene, count) %>%
  as.data.frame() %>%
  magrittr::set_rownames(NULL) %>%
  column_to_rownames("sample") %>%
  as.matrix

counts <- counts[, !(colnames(counts) %in% c("alignment_not_unique", "no_feature", "ambiguous"))]

# download cell info
geo <- GEOquery::getGEO("GSE60781", destdir = dataset_source_file(""))
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

grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))
