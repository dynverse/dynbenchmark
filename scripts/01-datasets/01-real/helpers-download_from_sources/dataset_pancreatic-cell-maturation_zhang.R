library(tidyverse)
library(dynbenchmark)
library(GEOquery)

dataset_preprocessing("real/gold/pancreatic-cell-maturation_zhang")

txt_location <- download_dataset_source_file(
  "GSE87375_Single_Cell_RNA-seq_Gene_Read_Count.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE87375&format=file&file=GSE87375%5FSingle%5FCell%5FRNA%2Dseq%5FGene%5FRead%5FCount%2Etxt%2Egz"
)

allcounts <- read_tsv(txt_location) %>%
  as.data.frame() %>%
  column_to_rownames("ID") %>%
  select(-Symbol) %>%
  as.matrix() %>%
  t

# geo <- GEOquery::getGEO(GEO = "GSE87375", destdir = dataset_source_file(""))
# allcell_info <- geo[[2]] %>%
#   Biobase::phenoData() %>%
#   as("data.frame") %>%
#   rename(cell_id = title) %>%
#   mutate(milestone_id=gsub("batch: (.*)-\\d", "\\1", characteristics_ch1.5)) %>%
#   select(cell_id, milestone_id) %>%
#   mutate_all(funs(as.character))

# temporary workaround because the above does not seem to be working
geo <- GEOquery::getGEO(GEO = "GSE87375", destdir = dataset_source_file(""), GSEMatrix = FALSE)

procful <- function(x) {
  batch_str <- x[grepl("batch:", x)]
  if (length(batch_str) > 0) {
    gsub("batch: (.*)-\\d", "\\1", batch_str)
  } else {
    NA
  }
}

allcell_info <- dynutils::list_as_tibble(geo@gsms %>% map(~.@header)) %>%
  rowwise() %>%
  mutate(
    cell_id = title,
    milestone_id = procful(characteristics_ch1)
  ) %>%
  ungroup() %>%
  select(cell_id, milestone_id) %>%
  mutate_all(funs(as.character))
# end temporary workaround

settings <- list(
  list(
    id = "real/gold/pancreatic-alpha-cell-maturation_zhang",
    milestone_network = tribble(
      ~from, ~to, ~length,
      "α-cell E17.5", "α-cell P0", 2.5,
      "α-cell P0", "α-cell P9", 9,
      "α-cell P9", "α-cell P15", 6,
      "α-cell P15", "α-cell P18", 3,
      "α-cell P18", "α-cell P60", 42
    ) %>% mutate(directed = TRUE)
  ),
  list(
    id = "real/gold/pancreatic-beta-cell-maturation_zhang",
    milestone_network = tribble(
      ~from, ~to, ~length,
      "β-cell E17.5", "β-cell P0", 2.5,
      "β-cell P0", "β-cell P3", 3,
      "β-cell P3", "β-cell P9", 6,
      "β-cell P9", "β-cell P15", 6,
      "β-cell P15", "β-cell P18", 3,
      "β-cell P18", "β-cell P60", 42
    ) %>% mutate(directed = TRUE)
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
