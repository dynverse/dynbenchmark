library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/silver/fibroblast-reprogramming_treutlein")

txt_location <- download_dataset_source_file(
  "GSE67310_iN_data_log2FPKM_annotated.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67310&format=file&file=GSE67310%5FiN%5Fdata%5Flog2FPKM%5Fannotated%2Etxt%2Egz"
)

df <- read_tsv(txt_location, col_types = cols(cell_name = "c", assignment = "c", experiment = "c", time_point = "c", .default = "d"))
expression <- df[, -c(1:5)] %>% as.matrix() %>% magrittr::set_rownames(df$cell_name)
cell_info <- df[, c(1:5)] %>% as.data.frame() %>% magrittr::set_rownames(df$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = assignment
  )

milestone_network = tribble(
  ~from, ~to,
  "MEF", "d2_intermediate",
  "d2_intermediate", "d2_induced",
  "d2_induced", "d5_intermediate",
  "d5_intermediate", "d5_earlyiN",
  "d5_earlyiN", "Neuron",
  "d5_earlyiN", "Myocyte"
) %>% mutate(length = 1, directed = TRUE)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

counts <- floor(2^expression-1)

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]

grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))
