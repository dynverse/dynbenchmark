rm(list=ls())
library(tidyverse)
options('download.file.method.GEOquery'='curl')

txt_web_location <- "http://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE75330&format=file&file=GSE75330%5FMarques%5Fet%5Fal%5Fmol%5Fcounts2%2Etab%2Egz"
txt_location <- "analysis/data/datasets_preproc/Marques_mol_counts.tab.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl")
}

allcounts <- read_tsv(txt_location) %>% tibble::column_to_rownames("cellid") %>% as.matrix() %>% t

geo <- GEOquery::getGEO("GSE75330", destdir="analysis/data/datasets_preproc/")
allcell_info <- geo[[1]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  select(title, characteristics_ch1.4) %>%
  rename(cell_id=title, milestone_id=characteristics_ch1.4) %>%
  mutate(
    cluster=gsub("inferred cell type: ([A-Z]*)\\d*", "\\1", milestone_id, perl=TRUE),
    subcluster=gsub("inferred cell type: ([A-Z1-9]*)", "\\1", milestone_id)) %>%
  mutate_all(funs(as.character))

info <- list(id="oligodendrocyte_differentiation_marques")

settings <- list(
  list(
    id = "oligodendrocyte_differentiation_marques_subclusters",
    milestone_source = "subcluster",
    milestone_network = tribble(
      ~from, ~to,
      "OPC", "COP",
      "COP", "NFOL1",
      "NFOL1", "NFOL2",
      "NFOL2", "MFOL1",
      "MFOL1", "MFOL2",
      "MFOL2", "MOL1",
      "MFOL2", "MOL2",
      "MFOL2", "MOL3",
      "MFOL2", "MOL4",
      "MFOL2", "MOL5",
      "MFOL2", "MOL6"
    )
  ),
  list(
    id = "oligodendrocyte_differentiation_marques_clusters",
    milestone_source = "cluster",
    milestone_network = tribble(
      ~from, ~to,
      "OPC", "COP",
      "COP", "NFOL",
      "NFOL", "MOL"
    )
  )
)

for (setting in settings) {
  info <- list(id = setting$id)
  milestone_network <- setting$milestone_network
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids) %>% filter(cell_id %in% rownames(allcounts))
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id = group_id) %>% mutate(percentage = 1)

  gene_info <- tibble(id = colnames(counts))
  gene_ids <- gene_info$id

  expression <- log2(counts + 1)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, expression, counts, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}
