# problem with the dataset: incomplete cluster labels! Several clusters are not labelled in the file on github


library(tidyverse)
library(dynbenchmark)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/olfactory-epithelium-differentiation_fletcher")

# counts
counts_file <- download_dataset_source_file(
  "GSE95601_oeHBCdiff_RSEM_eSet_expectedCounts_table.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE95601&format=file&file=GSE95601%5FoeHBCdiff%5FRSEM%5FeSet%5FexpectedCounts%5Ftable%2Etxt%2Egz"
)

counts_file_unzipped <- gsub("\\.gz", "", counts_file)
read_lines(counts_file) %>% {.[[1]] <- paste0("\"gene\"\t", .[[1]]); .} %>% write_lines(counts_file_unzipped)

counts_all <- read_tsv(counts_file_unzipped) %>%
  as.data.frame() %>%
  column_to_rownames("gene") %>%
  as.matrix() %>%
  t()
counts_all <- counts_all[, apply(counts_all, 2, function(x) !any(is.na(x)))]
counts_all <- counts_all[, !(str_detect(colnames(counts_all), "^ERCC-.*"))]

# cell info
cell_info_all <- read_tsv("https://raw.githubusercontent.com/rufletch/p63-HBC-diff/master/ref/oeHBCdiff_clusterLabels.txt", col_names = c("cell_id", "group_ix"))
groups <- read_delim("https://raw.githubusercontent.com/rufletch/p63-HBC-diff/master/ref/clusterKey.md", col_names = c("group_ix", "group_id"), delim = ".") %>% mutate(group_id = trimws(gsub("Cluster [0-9]*: (.*)", "\\1", group_id)))
cell_info_all <- cell_info_all %>% left_join(groups, "group_ix")

milestone_network <- tribble(
  ~from, ~to,
  "Resting Horizontal Basal Cells (HBCs)", "Transitional HBC 1",
  "Transitional HBC 1", "Transitional HBC 2",
  "Transitional HBC 2", "Immature Sustentacular Cells",
  "Immature Sustentacular Cells", "Mature Sustentacular Cells",
  "Transitional HBC 2", "Globose Basal Cells (GBCs)",
  "Globose Basal Cells (GBCs)", "Microvillous Cells, type 1",
  "Microvillous Cells, type 1", "Microvillous Cells, type 2",
  "Globose Basal Cells (GBCs)", "Immediate Neuronal Precursor 1 (INP1)",
  "Immediate Neuronal Precursor 1 (INP1)", "Immediate Neuronal Precursor 2 (INP2)",
  "Immediate Neuronal Precursor 2 (INP2)", "Immediate Neuronal Precursor 3 (INP3)",
  "Immediate Neuronal Precursor 3 (INP3)", "Immature Olfacotry Sensory Neurons (iOSNs)",
  "Immature Olfacotry Sensory Neurons (iOSNs)", "mature Olfactory Sensory Neurons (mOSNs)"
) %>% mutate(length = 1, directed = TRUE)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id)) %>%
  filter(group_id %in% milestone_ids)
counts <- counts_all[cell_info$cell_id, ]

grouping <- cell_info %>% select(cell_id, group_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))



process_raw_dataset(get_dataset_preprocessing_id(), counts, milestone_network, grouping)
