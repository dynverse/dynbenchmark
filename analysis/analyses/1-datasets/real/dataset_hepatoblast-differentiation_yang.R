rm(list=ls())
library(tidyverse)
library(dynalysis)

dataset_preprocessing("real/hepatoblast-differentiation_yang")

txt_location <- download_dataset_file(
  "GSE90047_Single-cell_RNA-seq_TPM.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE90047&format=file&file=GSE90047%5FSingle%2Dcell%5FRNA%2Dseq%5FTPM%2Etxt%2Egz"
)

counts <- read_tsv(txt_location, col_types = cols(ID = "c", Symbol = "c", .default = "d")) %>%
  as.data.frame() %>%
  column_to_rownames("ID") %>%
  select(-Symbol) %>%
  as.matrix() %>%
  t

geo <- GEOquery::getGEO(GEO = "GSE90047", destdir = dataset_preproc_file(""))
cell_info <- geo[[2]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  mutate(
    day = gsub("embryonic day: (.*)", "\\1", characteristics_ch1),
    putative_cell_type = gsub("putative cell type: (.*)", "\\1", characteristics_ch1.4)
  ) %>%
  select(cell_id = title, day, putative_cell_type) %>%
  mutate_all(funs(as.character)) %>%
  mutate(
    milestone_id = ifelse(day %in% c("E10.5", "E11.5", "E12.5"), day, paste0(day, "#", putative_cell_type))
  )

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "E10.5", "E11.5", 1, TRUE,
  "E11.5", "E12.5", 1, TRUE,
  "E12.5", "E13.5#hepatoblast/hepatocyte", 1, TRUE,
  "E12.5", "E13.5#cholangiocyte", 1, TRUE,
  "E13.5#cholangiocyte","E14.5#cholangiocyte", 1, TRUE,
  "E14.5#cholangiocyte","E15.5#cholangiocyte", 1, TRUE,
  "E15.5#cholangiocyte","E17.5#cholangiocyte", 2, TRUE,
  "E13.5#hepatoblast/hepatocyte","E14.5#hepatoblast/hepatocyte", 1, TRUE,
  "E14.5#hepatoblast/hepatocyte","E15.5#hepatoblast/hepatocyte", 1, TRUE,
  "E15.5#hepatoblast/hepatocyte","E17.5#hepatoblast/hepatocyte", 2, TRUE
)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>%
  slice(match(rownames(counts), cell_id)) %>%
  filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>%
  rename(milestone_id = group_id) %>%
  mutate(percentage = 1)

feature_info <- tibble(feature_id = colnames(counts))

datasetpreproc_normalise_filter_wrap_and_save(
  ti_type = "bifurcating",
  counts = counts,
  cell_ids = cell_ids,
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  milestone_percentages = milestone_percentages,
  cell_grouping = cell_grouping,
  cell_info = cell_info,
  feature_info = feature_info
)
