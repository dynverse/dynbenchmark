library(dynbenchmark)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/silver/dentate-gyrus-neurogenesis_hochgerner")


# get cell info
geo <- GEOquery::getGEO("GSE95315", destdir = dataset_source_file(""))[[1]]

cell_info_all <- phenoData(geo) %>% as("data.frame") %>% select(title, characteristics_ch1.2, characteristics_ch1.4) %>%
  rename(
    cell_id = title,
    time_id = characteristics_ch1.2,
    population_id = characteristics_ch1.4
  ) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(population_id = gsub("cell cluster: (.*)", "\\1", population_id)) %>%
  mutate(time_id = gsub("postnatal day: (.*)", "\\1", time_id))

# merge some clusters as in the paper
merge_clusters <- setNames(unique(cell_info_all$population_id), unique(cell_info_all$population_id))
merge_clusters["nIPC2"] <- "nIPC"
cell_info_all$population_id <- merge_clusters[cell_info_all$population_id]

# load and process all counts
tab_location <- download_dataset_source_file(
  "GSE95315_10X_expression_data.tab.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE95315&format=file&file=GSE95315%5F10X%5Fexpression%5Fdata%2Etab%2Egz"
)

counts_all_preproc <- read_tsv(tab_location)
gene_ids <- counts_all_preproc$cellid
counts_all_preproc <- as.matrix(counts_all_preproc[, -1])
rownames(counts_all_preproc) <- gene_ids
counts_all <- t(counts_all_preproc)

# construct dataset
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "nIPC", "Neuroblast", 1, T,
  "Neuroblast", "Neuroblast2", 1, T,
  "Neuroblast2", "Granule-immature", 1, T,
  "Granule-immature", "Granule-mature", 1, T
)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info_all %>% slice(match(rownames(counts_all), cell_id)) %>%
  rename(milestone_id = population_id) %>%
  filter(milestone_id %in% milestone_ids)

counts <- counts_all[cell_info$cell_id, ]

grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))
