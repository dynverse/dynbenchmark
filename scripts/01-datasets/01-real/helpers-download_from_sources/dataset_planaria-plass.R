library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("real/silver/whole-schmidtea-mediterranea_plass")

# get settings
source(paste0(dynbenchmark::get_dynbenchmark_folder(), "/scripts/01-datasets/01-real/helpers-download_from_sources/helper_planaria-plass.R"))

# counts
counts_file <- download_dataset_source_file(
  "dge.txt.gz",
  "http://bimsbstatic.mdc-berlin.de/rajewsky/PSCA/dge.txt.gz"
)

counts_file_unzipped <- gsub("\\.gz", "", counts_file)
read_lines(counts_file) %>% {.[[1]] <- paste0("\"gene\"\t", .[[1]]); .} %>% write_lines(counts_file_unzipped)

counts_all <- read_tsv(counts_file_unzipped) %>%
  as.data.frame() %>%
  column_to_rownames("gene") %>%
  as.matrix() %>%
  t

# cell info
cell_info_file <- download_dataset_source_file(
  "R_annotation.txt",
  "http://bimsbstatic.mdc-berlin.de/rajewsky/PSCA/R_annotation.txt"
)
cell_info_all <- tibble(group_id = read_lines(cell_info_file))
cell_info_all$cell_id <- rownames(counts_all)


for (setting in settings) {
  print(setting$id)

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  if (!all(milestone_ids %in% cell_info_all$group_id)) {stop(setdiff(milestone_ids, cell_info_all$group_id))}

  if (!"length" %in% names(milestone_network)) milestone_network$length <- 1
  milestone_network$length[is.na(milestone_network$length)] <- TRUE
  if (!"directed" %in% names(milestone_network)) milestone_network$directed <- TRUE
  milestone_network$directed[is.na(milestone_network$directed)] <- TRUE

  cell_info <- cell_info_all %>%
    filter(group_id %in% milestone_ids)

  grouping <- cell_info %>%
    select(cell_id, group_id) %>%
    deframe()

  counts <- counts_all[names(grouping), ]

  save_raw_dataset(
    lst(
    milestone_network,
    grouping,
    cell_info,
    counts
  ),
  setting$id)
}
