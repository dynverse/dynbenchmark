rm(list=ls())
library(dynalysis)
library(tidyverse)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/oligodendrocyte_differentiation_marques")

txt_location <- download_dataset_file(
  "Marques_mol_counts.tab.gz",
  "http://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE75330&format=file&file=GSE75330%5FMarques%5Fet%5Fal%5Fmol%5Fcounts2%2Etab%2Egz"
)

allcounts <- read_tsv(txt_location) %>% as.data.frame %>% tibble::column_to_rownames("cellid") %>% as.matrix() %>% t

geo <- GEOquery::getGEO("GSE75330", destdir=dataset_preproc_file(""))
allcell_info <- geo[[1]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  select(title, characteristics_ch1.4) %>%
  rename(cell_id = title, milestone_id = characteristics_ch1.4) %>%
  mutate(
    cluster = gsub("inferred cell type: ([A-Z]*)\\d*", "\\1", milestone_id, perl=TRUE),
    subcluster = gsub("inferred cell type: ([A-Z1-9]*)", "\\1", milestone_id)
  ) %>%
  mutate_all(funs(as.character))

settings <- list(
  list(
    id = "real/oligodendrocyte_differentiation_subclusters_marques",
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
    ) %>% mutate(length = 1, directed = TRUE),
    ti_type = "tree"
  ),
  list(
    id = "real/oligodendrocyte_differentiation_clusters_marques",
    milestone_source = "cluster",
    milestone_network = tribble(
      ~from, ~to,
      "OPC", "COP",
      "COP", "NFOL",
      "NFOL", "MOL"
    ) %>% mutate(length = 1, directed = TRUE),
    ti_tpye = "linear"
  )
)

for (setting in settings) {
  dataset_preprocessing(setting$id)

  milestone_network <- setting$milestone_network
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids) %>% filter(cell_id %in% rownames(allcounts))
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id = group_id) %>% mutate(percentage = 1)

  feature_info <- tibble(feature_id = colnames(counts))

  datasetpreproc_normalise_filter_wrap_and_save(
    ti_type = setting$ti_type,
    counts = counts,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )
}
