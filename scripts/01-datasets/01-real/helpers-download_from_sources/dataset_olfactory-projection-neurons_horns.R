library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("real/goldsilver/olfactory-projection-neurons_horns")

txts <- tibble(remote_location=c(
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DA1_24hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DA1_36hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DA1_48hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DA1_72hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DA1_adult.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DC3_24hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DC3_36hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DC3_48hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_DC3_72hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_VA1d_24hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_VA1d_36hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_VA1d_48hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_VA1d_72hAPF.txt",
  "https://raw.githubusercontent.com/felixhorns/FlyPN/master/data/names_adPN_adult.txt"
)) %>%
  mutate(
    file = gsub(".*/(.*)$", "\\1", remote_location)
)
txts <- bind_cols(txts, str_match(txts$file, "names_(.*)_(.*)\\.txt")[, c(2, 3)] %>% as.data.frame() %>% magrittr::set_colnames(c("celltype", "stage")))
txts$location <- map2_chr(txts$file, txts$remote_location, download_dataset_source_file)
txts$cell_id <- map(txts$location, readLines)

allcell_info <- txts %>% unnest(cell_id) %>% select(cell_id, celltype, stage) %>% mutate_if(is.factor, as.character) %>% mutate(group_id = glue::glue("{celltype}_{stage}"))

counts_location <- download_dataset_source_file(
  "GSE100058_htseq.tab.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE100058&format=file&file=GSE100058%5Fhtseq%2Etab%2Egz"
)

allcounts <- read_csv(counts_location) %>% as.data.frame %>% tibble::column_to_rownames("symbol") %>% as.matrix() %>% t
allcounts <- allcounts[, !(colnames(allcounts) %in% c("GAL4", "flp","mCD8-GFP", "__no_feature", "__ambiguous", "__too_low_aQual", "__not_aligned", "__alignment_not_unique"))]
allcounts <- allcounts[, !(str_detect(colnames(allcounts), "^ERCC-.*"))]

settings <- list(
  list(
    id = "real/silver/olfactory-projection-neurons-DA1_horns",
    milestone_source = "group_id",
    milestone_network = tribble(
      ~from, ~to,
      "DA1_24hAPF", "DA1_36hAPF",
      "DA1_36hAPF", "DA1_48hAPF",
      "DA1_48hAPF", "DA1_72hAPF",
      "DA1_72hAPF", "DA1_adult"
    ) %>% mutate(length = 1, directed = TRUE)
  ),
  list(
    id = "real/silver/olfactory-projection-neurons-DC3_VA1d_horns",
    milestone_source = "group_id",
    milestone_network = tribble(
      ~from, ~to,
      "DC3_24hAPF", "DC3_36hAPF",
      "DC3_36hAPF", "DC3_48hAPF",
      "DC3_48hAPF", "DC3_72hAPF",
      "DC3_72hAPF", "adPN_adult",
      "VA1d_24hAPF", "VA1d_36hAPF",
      "VA1d_36hAPF", "VA1d_48hAPF",
      "VA1d_48hAPF", "VA1d_72hAPF",
      "VA1d_72hAPF", "adPN_adult"
    ) %>% mutate(length = 1, directed = TRUE)
  ),
  list(
    id = "real/silver/olfactory-projection-neurons_horns",
    milestone_source = "group_id",
    milestone_network = tribble(
      ~from, ~to,
      "DC3_24hAPF", "DC3_36hAPF",
      "DC3_36hAPF", "DC3_48hAPF",
      "DC3_48hAPF", "DC3_72hAPF",
      "DC3_72hAPF", "adPN_adult",
      "VA1d_24hAPF", "VA1d_36hAPF",
      "VA1d_36hAPF", "VA1d_48hAPF",
      "VA1d_48hAPF", "VA1d_72hAPF",
      "VA1d_72hAPF", "adPN_adult",
      "DA1_24hAPF", "DA1_36hAPF",
      "DA1_36hAPF", "DA1_48hAPF",
      "DA1_48hAPF", "DA1_72hAPF",
      "DA1_72hAPF", "DA1_adult"
    ) %>% mutate(length = 1, directed = TRUE)
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids) %>% filter(cell_id %in% rownames(allcounts))
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
