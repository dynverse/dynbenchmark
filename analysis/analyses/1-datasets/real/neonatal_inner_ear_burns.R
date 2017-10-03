rm(list=ls())

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FRSEM%5FCounts%5FMatrix%2Etxt%2Egz"
count_location <- "analysis/data/datasets_preproc/GSE71982_RSEM_Counts_Matrix.txt.gz"

if (!file.exists(count_location)) {
  download.file(paste0(txt_web_location), count_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FP1%5FUtricle%5FPhenoData%2Etxt%2Egz"
phenodata_location <- "analysis/data/datasets_preproc/GSE71982_P1_Utricle_PhenoData.txt.gz"

if (!file.exists(phenodata_location)) {
  download.file(paste0(txt_web_location), phenodata_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

counts_all <- read_tsv(count_location)[-c(1, 2),] %>% column_to_rownames("X1") %>% as.matrix() %>% t
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
    ),
    id = "neonatal_inner_ear_all_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "SC (i)", "SC (ii)",
      "SC (ii)", "HC (ii)",
      "HC (ii)", "HC (iii-iv)"
    ),
    id = "neonatal_inner_ear_SC_HC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "SC (i)",
      "SC (i)", "SC (ii)"
    ),
    id = "neonatal_inner_ear_TEC_SC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "HC (i)",
      "HC (i)", "HC (iii-iv)"
    ),
    id = "neonatal_inner_ear_TEC_HSC_burns"
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
