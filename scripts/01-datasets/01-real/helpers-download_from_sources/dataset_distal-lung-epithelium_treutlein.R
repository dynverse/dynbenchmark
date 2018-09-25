library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("real/silver/distal-lung-epithelium_treutlein")

txt_location <- download_dataset_source_file(
  "nature13173-s4.txt",
  "http://www.nature.com/nature/journal/v509/n7500/extref/nature13173-s4.txt"
)

df <- read_tsv(txt_location, col_types = cols(cell_name = "c", time_point = "c", sample = "c", putative_cell_type = "c", .default = "d"))
expression <- df[, -c(1:4)] %>% as.matrix() %>% magrittr::set_rownames(df$cell_name)
cell_info <- df[, c(1:4)] %>% as.data.frame() %>% magrittr::set_rownames(df$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = putative_cell_type
  ) %>% filter(sample == "singleCell")
expression <- expression[cell_info$cell_id, ]

milestone_network = tribble(
  ~from, ~to,
  "BP", "AT1",
  "BP", "AT2"
) %>% mutate(length = 1, directed = TRUE)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

counts <- 2^expression-1

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]

grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))
