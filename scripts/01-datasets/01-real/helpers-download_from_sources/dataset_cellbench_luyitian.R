library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("real/gold/cellbench")

rda_location <- download_dataset_source_file(
  "9cellmix_qc.RData",
  "https://raw.githubusercontent.com/LuyiTian/CellBench_data/master/data/9cellmix_qc.RData"
)

settings <- list(
  list(
    object = sce_SC1_qc,
    id = "real/gold/cellbench-SC1_luyitian"
  ),
  list(
    object = sce_SC2_qc,
    id = "real/gold/cellbench-SC2_luyitian"
  ),
  list(
    object = sce_SC3_qc,
    id = "real/gold/cellbench-SC3_luyitian"
  ),
  list(
    object = sce_SC4_qc,
    id = "real/gold/cellbench-SC4_luyitian"
  )
)

for (setting in settings) {
  cell_info <- colData(setting$object) %>%
    as.data.frame() %>%
    rownames_to_column("cell_id") %>%
    mutate_at(vars(H1975, H2228, HCC827), ~.>0) %>%
    filter(traj == "YES") # why isn't this a logical?
  cell_info$group_id <- cell_info[, c("H1975", "H2228", "HCC827")] %>% apply(1, function(x) glue::glue_collapse(names(x)[x], ","))

  grouping <- cell_info %>% select(cell_id, group_id) %>% deframe()

  milestone_network <- tribble(
    ~from, ~to,
    "H1975", "H1975,H2228,HCC827",
    "HCC827", "H1975,H2228,HCC827",
    "H2228", "H1975,H2228,HCC827"
  ) %>% mutate(length = 1, directed = FALSE)

  counts <- t(counts(setting$object))[cell_info$cell_id, ]

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
