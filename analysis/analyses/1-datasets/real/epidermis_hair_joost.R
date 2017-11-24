rm(list=ls())
library(tidyverse)
library(dynalysis)
options('download.file.method.GEOquery'='curl')

id <- "epidermis_hair_spatial_joost"
dataset_preprocessing("real", id)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67602&format=file&file=GSE67602%5FJoost%5Fet%5Fal%5Fexpression%2Etxt%2Egz"
txt_location <- dataset_preproc_file("GSE67602_Joost_et_al_expression.txt.gz")

if (!file.exists(txt_location)) {
  download.file(txt_web_location, txt_location, method = "libcurl")
}

counts_all <- read_tsv(txt_location) %>% filter(!startsWith(`Gene\\Cell`, "ERCC")) %>% tibble::column_to_rownames("Gene\\Cell") %>% as.matrix() %>% t

geo <- GEOquery::getGEO("GSE67602", destdir = dataset_preproc_file())
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
      ~from, ~to, ~length, ~directed,
      "OB", "IB", 1, TRUE,
      "IB", "uHF-I", 1, TRUE,
      "uHF-I", "IFE-B", 1, TRUE
    ),
    id = "epidermis_hair_spatial_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "IFE-B", "IFE-DI", 1, TRUE,
      "IFE-DI", "IFE-DII", 1, TRUE,
      "IFE-DII", "IFE-KI", 1, TRUE,
      "IFE-KI", "IFE-KII", 1, TRUE
    ),
    id = "epidermis_hair_IFE_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "uHF-I", "uHF-II",  1, TRUE,
      "uHF-II", "uHF-III",  1, TRUE
    ),
    id = "epidermis_hair_uHF_joost"
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  feature_info <- tibble(feature_id = colnames(counts))

  # TODO: use dynutils normalisation
  expression <- log2(counts + 1)

  dataset <- wrap_ti_task_data(
    ti_type = "real",
    id = setting$id,
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )

  save_dataset(dataset)
}
