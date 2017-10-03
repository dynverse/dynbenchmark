rm(list=ls())
library(tidyverse)
options('download.file.method.GEOquery'='curl')

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE90860&format=file&file=GSE90860%5FSupplementaryData1%5Frevised%5Fjulien%2Etsv%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE90860_SupplementaryData1_revised_julien.tsv.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

counts <- read_tsv(txt_location) %>% column_to_rownames("X1") %>% as.matrix() %>% t

geo <- GEOquery::getGEO(GEO="GSE90860", destdir="analysis/data/datasets_preproc/")
cell_info <- geo[[1]] %>% Biobase::phenoData() %>% as("data.frame") %>% mutate(cell_id=rownames(counts))
cell_info <- cell_info %>%
  mutate(milestone_id=paste0(gsub("age: (.*)", "\\1", characteristics_ch1), "#", gsub("assigned_subgroup: (.*)", "\\1", characteristics_ch1.1))) %>%
  select(cell_id, milestone_id) %>%
  mutate_all(funs(as.character))

info <- list(id="cortical_interneuron_differentiation_frazer")

milestone_network <- tribble(
  ~from, ~to, ~length,
  "E18#0", "P2#1", 4,
  "E18#0", "P2#2", 4,
  "E18#0", "P2#3", 4,
  "P2#1", "P5#1", 3,
  "P2#2", "P5#2", 3,
  "P2#3", "P5#3", 3
)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

gene_info <- tibble(id=colnames(counts))
gene_ids <- gene_info$id

expression <- log2(counts + 1)

dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, expression, counts, milestone_network, milestone_ids, milestone_percentages, info)

dynalysis:::save_dataset(dataset)
