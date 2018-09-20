library(tidyverse)
library(dynbenchmark)
library(GEOquery)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/goldsilver/hematopoiesis_olsson")

files_df <- tribble(
  ~location, ~url,
  "GSE70240_Gmp.txt.gz", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70240&format=file&file=GSE70240%5FGmp%2Etxt%2Egz",
  "GSE70243_LK.CD34+.txt.gz", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70243&format=file&file=GSE70243%5FLK%2ECD34%2B%2Etxt%2Egz",
  "GSE70244_Lsk.txt.gz", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70244&format=file&file=GSE70244%5FLsk%2Etxt%2Egz",
  "GSE70236_Cmp.txt.gz", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70236&format=file&file=GSE70236%5FCmp%2Etxt%2Egz"
) %>%
  rowwise() %>%
  mutate(
    preproc_loc = download_dataset_source_file(location, url)
  ) %>%
  ungroup()

allexpression <- files_df$preproc_loc %>%
  map(~ read_tsv(., col_types = cols(uid = "c", .default = "d")) %>% gather(feature, expression, -uid)) %>%
  bind_rows() %>%
  spread(feature, expression) %>%
  as.data.frame() %>%
  magrittr::set_rownames(NULL) %>%
  column_to_rownames("uid") %>%
  as.matrix %>%
  t

allcounts <- 2^allexpression-1

txt_location <- download_dataset_source_file(
  "nature19348-f1.xlsx",
  "https://images.nature.com/original/nature-assets/nature/journal/v537/n7622/source_data/nature19348-f1.xlsx"
)

small_expression <- readxl::read_xlsx(txt_location, 1)
subclusters <- small_expression[1, -c(1, 2)] %>% as.list() %>% unlist()
subclusters_mapper <- c("HSCP-1", "HSCP-2", "Meg", "Eryth", "Multi-Lin", "MDP", "Mono", "Gran", "Myelocyte")
subclusters <- subclusters_mapper[subclusters] %>% set_names(names(subclusters))

allcell_info <- tibble(
  cell_id = names(subclusters) %>% gsub(".*:(.*)", "\\1", .),
  cluster = subclusters,
  gate = names(subclusters) %>% gsub("(.*):.*", "\\1", .)
)

settings <- list(
  list(
    id = "real/silver/hematopoiesis-clusters_olsson",
    milestone_source = "cluster",
    milestone_network = tribble(
      ~from, ~to,
      "HSCP-1", "HSCP-2",
      "HSCP-2", "Multi-Lin",
      "Multi-Lin", "MDP",
      "Multi-Lin", "Eryth",
      "Multi-Lin", "Meg",
      "MDP", "Gran",
      "MDP", "Mono",
      "Gran", "Myelocyte"
    ) %>% mutate(length = 1, directed = TRUE)
  ),
  list(
    id = "real/gold/hematopoiesis-gates_olsson",
    milestone_source = "gate",
    milestone_network = tribble(
      ~from, ~to,
      "Lsk", "Cmp",
      "Cmp", "Gmp"
    ) %>% mutate(length = 1, directed = TRUE)
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
