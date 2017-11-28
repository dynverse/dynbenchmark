rm(list=ls())
library(tidyverse)
library(dynalysis)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/stimulated-dendritic-cells_shalek")

file <- download_dataset_file(
  "GSM1406531_GSM1407094.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE48968&format=file&file=GSE48968%5FallgenesTPM%5FGSM1189042%5FGSM1190902%2Etxt%2Egz"
)
tab <- read.table(gzfile(file), header = TRUE, sep = "\t") %>% as.matrix %>% t

allcell_info <- data_frame(
  cell_id = rownames(tab),
  time = ifelse(grepl("_[0-9]+h", cell_id), gsub(".*_([0-9]+h).*", "\\1", cell_id), "0h"),
  type = ifelse(grepl("_[0-9]+h", cell_id), gsub("(.*)_[0-9]+h.*", "\\1", cell_id), "Unstimulated"),
  sid = ifelse(grepl("_S[0-9]+$", cell_id), gsub("_(S[0-9]+)$", "\\1", cell_id), ""),
  milestone_id = paste0(type, "#", time)
) %>% filter(
  type %in% c("LPS", "PAM", "PIC", "Unstimulated")
)

settings <- lapply(c("LPS", "PAM", "PIC"), function(stim) {
  list(
    id = pritt("real/stimulated-dendritic-cells-{stim}_shalek"),
    milestone_network = tribble(
      ~from, ~to, ~length,
      "Unstimulated#0h", pritt("{stim}#1h"), 1,
      pritt("{stim}#1h"), pritt("{stim}#2h"), 1,
      pritt("{stim}#2h"), pritt("{stim}#4h"), 2,
      pritt("{stim}#4h"), pritt("{stim}#6h"), 2
    ) %>% mutate(directed = TRUE),
    ti_type = "linear"
  )
})

tab <- tab[, !grepl("^ERCC", colnames(tab))] # remove spike-ins, are not present in every cell

for (setting in settings) {
  dataset_preprocessing(setting$id)

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- tab[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, group_id = milestone_id)
  milestone_percentages <- cell_info %>% select(cell_id, milestone_id) %>% mutate(percentage = 1)

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
