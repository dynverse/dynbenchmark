rm(list=ls())
library(tidyverse)
library(dynalysis)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/epidermis-hair_joost")

txt_location <- download_dataset_file(
  "GSE67602_Joost_et_al_expression.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67602&format=file&file=GSE67602%5FJoost%5Fet%5Fal%5Fexpression%2Etxt%2Egz"
)

counts_all <- read_tsv(txt_location) %>%
  as.data.frame() %>%
  filter(!startsWith(`Gene\\Cell`, "ERCC")) %>%
  tibble::column_to_rownames("Gene\\Cell") %>%
  as.matrix() %>%
  t

geo <- GEOquery::getGEO("GSE67602", destdir = dataset_preproc_file(""))
cell_info_all <- geo[[1]] %>%
  Biobase::phenoData() %>%
  as("data.frame") %>%
  select(title, characteristics_ch1) %>%
  rename(cell_id = title, milestone_id = characteristics_ch1) %>%
  mutate(milestone_id=gsub("cell type level 1: (.*)", "\\1", milestone_id)) %>%
  mutate_all(funs(as.character))

settings <- list(
  list(
    # this ordering is different than in the paper,
    # but spatially speaking, OB is first and IB second, so this makes much more sense
    milestone_network = tribble(
      ~from, ~to,
      "OB", "IB",
      "IB", "uHF-I",
      "uHF-I", "IFE-B"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/epidermis-hair-spatial_joost",
    trajectory_type = "linear"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "IFE-B", "IFE-DI",
      "IFE-DI", "IFE-DII",
      "IFE-DII", "IFE-KI",
      "IFE-KI", "IFE-KII"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/epidermis-hair-IFE_joost",
    trajectory_type = "linear"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "uHF-I", "uHF-II",
      "uHF-II", "uHF-III"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/epidermis-hair-uHF_joost",
    trajectory_type = "linear"
  )
)

for (setting in settings) {
  dataset_preprocessing(setting$id)

  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  feature_info <- tibble(feature_id = colnames(counts))

  datasetpreproc_normalise_filter_wrap_and_save(
    trajectory_type = setting$trajectory_type,
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
