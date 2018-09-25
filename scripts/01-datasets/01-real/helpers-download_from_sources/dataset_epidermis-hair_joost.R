library(tidyverse)
library(dynbenchmark)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/silver/epidermis-hair_joost")

txt_location <- download_dataset_source_file(
  "GSE67602_Joost_et_al_expression.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67602&format=file&file=GSE67602%5FJoost%5Fet%5Fal%5Fexpression%2Etxt%2Egz"
)

counts_all <- read_tsv(txt_location) %>%
  as.data.frame() %>%
  filter(!startsWith(`Gene\\Cell`, "ERCC")) %>%
  tibble::column_to_rownames("Gene\\Cell") %>%
  as.matrix() %>%
  t

geo <- GEOquery::getGEO("GSE67602", destdir = dataset_source_file(""))
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
    id = "real/silver/epidermis-hair-spatial_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "IFE-B", "IFE-DI",
      "IFE-DI", "IFE-DII",
      "IFE-DII", "IFE-KI",
      "IFE-KI", "IFE-KII"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/epidermis-hair-IFE_joost"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "uHF-I", "uHF-II",
      "uHF-II", "uHF-III"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/epidermis-hair-uHF_joost"
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id)) %>%
    filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
