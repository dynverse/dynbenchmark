rm(list=ls())
library(tidyverse)
options('download.file.method.GEOquery'='curl')

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67602&format=file&file=GSE67602%5FJoost%5Fet%5Fal%5Fexpression%2Etxt%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE67602_Joost_et_al_expression.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}

counts_all <- read_tsv(txt_location) %>% filter(!startsWith(`Gene\\Cell`, "ERCC")) %>% tibble::column_to_rownames("Gene\\Cell") %>% as.matrix() %>% t

geo <- GEOquery::getGEO("GSE67602", destdir="analysis/data/datasets_preproc/")
cell_info_all <- geo[[1]] %>% Biobase::phenoData() %>% as("data.frame") %>% select(title, characteristics_ch1) %>% rename(cell_id=title, milestone_id=characteristics_ch1) %>% mutate(milestone_id=gsub("cell type level 1: (.*)", "\\1", milestone_id)) %>% mutate_all(funs(as.character))

settings <- list(
  list(
    milestone_network = tribble(
      ~from, ~to,
      "OB", "IB", # this ordering is different than in the paper, but spatially speaking, OB is first and IB second, so this makes much more sense
      "IB", "uHF-I",
      "uHF-I", "IFE-B"
    ),
    id = "epidermis_hair_spatial_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "IFE-B", "IFE-DI",
      "IFE-DI", "IFE-DII",
      "IFE-DII", "IFE-KI",
      "IFE-KI", "IFE-KII"
    ),
    id = "epidermis_hair_IFE_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "uHF-I", "uHF-II",
      "uHF-II", "uHF-III"
    ),
    id = "epidermis_hair_uHF_joost"
  )
)

for (setting in settings) {
  info <- list(id=setting$id)

  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  gene_info <- tibble(id=colnames(counts))
  gene_ids <- gene_info$id

  expression <- log2(counts + 1)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}
