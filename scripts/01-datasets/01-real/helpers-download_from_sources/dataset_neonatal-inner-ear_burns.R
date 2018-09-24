library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/silver/neonatal-inner-ear_burns")

count_location <- download_dataset_source_file(
  "GSE71982_RSEM_Counts_Matrix.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FRSEM%5FCounts%5FMatrix%2Etxt%2Egz"
)

phenodata_location <- download_dataset_source_file(
  "GSE71982_P1_Utricle_PhenoData.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FP1%5FUtricle%5FPhenoData%2Etxt%2Egz"
)

counts_all <- read_tsv(count_location)[-c(1, 2),] %>% as.data.frame %>%  column_to_rownames("X1") %>% as.matrix() %>% t
colnames(counts_all) <- colnames(counts_all) %>% gsub("[\\\\\"]*([^\\\\\"]*)[\\\\\"]*", "\\1",.)
cell_info_all <- read_tsv(phenodata_location) %>% rename(cell_id=Short_Name, milestone_id=GroupID)

settings <- list(
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "SC (i)",
      "SC (i)", "SC (ii)",
      "SC (ii)", "HC (ii)",
      "HC (ii)", "HC (iii-iv)",
      "TEC", "HC (i)",
      "HC (i)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/neonatal-inner-ear-all_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "SC (i)", "SC (ii)",
      "SC (ii)", "HC (ii)",
      "HC (ii)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/neonatal-inner-ear-SC-HC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "SC (i)",
      "SC (i)", "SC (ii)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/neonatal-inner-ear-TEC-SC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "HC (i)",
      "HC (i)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "real/silver/neonatal-inner-ear-TEC-HSC_burns"
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
