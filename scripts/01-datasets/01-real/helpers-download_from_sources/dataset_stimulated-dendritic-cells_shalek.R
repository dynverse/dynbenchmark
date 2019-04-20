library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/gold/stimulated-dendritic-cells_shalek")

file <- download_dataset_source_file(
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
    id = stringr::str_glue("real/gold/stimulated-dendritic-cells-{stim}_shalek"),
    milestone_network = tribble(
      ~from, ~to, ~length,
      "Unstimulated#0h", stringr::str_glue("{stim}#1h"), 1,
      stringr::str_glue("{stim}#1h"), stringr::str_glue("{stim}#2h"), 1,
      stringr::str_glue("{stim}#2h"), stringr::str_glue("{stim}#4h"), 2,
      stringr::str_glue("{stim}#4h"), stringr::str_glue("{stim}#6h"), 2
    ) %>% mutate(directed = TRUE)
  )
})

tab <- tab[, !grepl("^ERCC", colnames(tab))] # remove spike-ins, are not present in every cell

for (setting in settings) {
  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- tab[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
