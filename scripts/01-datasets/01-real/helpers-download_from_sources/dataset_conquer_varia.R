library(tidyverse)
library(dynbenchmark)
library(dynwrap)

dataset_preprocessing("real/goldsilver/conquer_varia")

requireNamespace("MultiAssayExperiment")

addcols <- function(x) x %>% mutate(length = 1, directed = TRUE)

conquer_infos <- list(
  list(
    id = "real/gold/cell-cycle_buettner",
    rds_name = "EMTAB2805",
    milestone_source = "cell_cycle_stage",
    milestone_network = tribble(
      ~from, ~to,
      "G1", "S",
      "S", "G2M",
      "G2M", "G1"
    ) %>% addcols,
    source_id = "conquer"
  ),
  list(
    id = "real/gold/human-embryos_petropoulos",
    rds_name = "EMTAB3929",
    milestone_source = "Characteristics.developmental.stage.",
    milestone_network = tribble(
      ~from, ~to,
      "embryonic day 3", "embryonic day 4",
      "embryonic day 4", "embryonic day 5",
      "embryonic day 5", "embryonic day 6",
      "embryonic day 6", "embryonic day 7"
    ) %>% addcols,
    source_id = "conquer"
  ),
  list(
    id = "real/gold/NKT-differentiation_engel",
    rds_name = "GSE74596",
    milestone_source = "characteristics_ch1.5",
    milestone_network = tribble(
      ~from, ~to,
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT1",
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT17",
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT2"
    ) %>% addcols,
    source_id = "conquer",
    remove_spike_ins = TRUE
  ),
  list(
    id = "real/silver/cell-cycle_leng",
    rds_name = "GSE64016",
    milestone_source = function(cell_info) gsub("(.*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "G1", "S",
      "S", "G2",
      "G2", "G1"
    ) %>% addcols,
    remove_spike_ins = TRUE
  ),
  list(
    id = "real/gold/mesoderm-development_loh",
    rds_name = "SRP073808",
    milestone_source = "LibraryName",
    milestone_network = tribble( # see http://ars.els-cdn.com/content/image/1-s2.0-S0092867416307401-fx1_lrg.jpg
      ~from, ~to,
      "H7hESC", "H7_derived_APS",
      "H7hESC", "H7_derived_MPS",
      "H7_derived_APS", "H7_derived_DLL1pPXM",
      "H7_derived_DLL1pPXM", "H7_dreived_D2.25_Smtmrs",
      "H7_dreived_D2.25_Smtmrs", "H7_derived_ESMT",
      "H7_derived_ESMT", "H7_derived_D5CntrlDrmmtm",
      "H7_derived_ESMT", "H7_derived_Sclrtm",
      "H7_derived_MPS", "H7_derived_D2LtM",
      "H7_derived_D2LtM", "H7_derived_D3GARPpCrdcM"
    ) %>% addcols
  ),
  list(
    id = "real/gold/myoblast-differentiation_trapnell",
    rds_name = c("GSE52529-GPL11154", "GSE52529-GPL16791"),
    milestone_source = function(cell_info) gsub("Cell (T\\d*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "T0", "T24",
      "T24", "T48",
      "T48", "T72"
    ) %>% addcols,
    source_id = "conquer"
  ),
  list(
    id = "real/gold/germline-human-female_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "F4W", "F8W", 4, TRUE,
      "F8W", "F10W", 2, TRUE,
      "F10W", "F11W", 1, TRUE,
      "F11W", "F17W", 6, TRUE
    ),
    milestone_source = function(cell_info) gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
  ),
  list(
    id = "real/gold/sgermline-human-male_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "M4W", "M7W", 3, TRUE,
      "M7W", "M10W", 3, TRUE,
      "M10W", "M11W", 1, TRUE,
      "M11W", "M19W", 8, TRUE
    ),
    milestone_source = function(cell_info) gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
  ),
  list(
    id = "real/gold/germline-human-both_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "4W", "7W", 3, TRUE,
      "7W", "8W", 1, TRUE,
      "8W", "10W", 2, TRUE,
      "10W", "11W", 1, TRUE,
      "11W", "F17W", 6, TRUE,
      "11W", "M19W", 8, TRUE
    ),
    milestone_source = function(cell_info) {
      src <- gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
      ifelse(src %in% c("F17W", "M19W"), src, gsub("[FM]", "", src))
    }
  )
)


for (source_info in conquer_infos) {
  id <- source_info$id
  cat(stringr::str_glue("Processing {id}"), sep = "\n")

  datas <- lapply(source_info$rds_name, function(rds_name) {
    rds_file <- download_dataset_source_file(
      stringr::str_glue("{rds_name}.rds"),
      stringr::str_glue("http://imlspenticton.uzh.ch/robinson_lab/conquer/data-mae/{rds_name}.rds")
    )
    read_rds(rds_file)
  })

  tran_counts <- do.call(rbind, lapply(datas, function(data) SummarizedExperiment::assay(data[["gene"]], "count") %>% t))

  # filter genes
  transcript_info <- datas %>%
    map_df(~ SummarizedExperiment::rowData(.[["gene"]]) %>% as.data.frame) %>%
    unique()

  if(!is.null(source_info$remove_spike_ins) && source_info$remove_spike_ins) {
    transcript_info <- transcript_info %>%
      filter(genome != "ERCC")
  }

  counts <- tran_counts[, transcript_info$gene] %>% t %>% rowsum(transcript_info$symbol) %>% t
  feature_info <- transcript_info %>%
    group_by(symbol) %>%
    summarise(ensembl_ids = paste(gene, collapse = ",")) %>%
    slice(match(symbol, colnames(counts))) %>%
    mutate_all(as.character) %>%
    select(feature_id = symbol, everything())

  # filter cells
  cell_info <- bind_rows(lapply(datas, function(data) {
    SummarizedExperiment::colData(data) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("cell_id") %>%
      mutate_if(is.factor, as.character)
  }))

  if (is.character(source_info$milestone_source)) {
    cell_info$milestone_id <- cell_info[[source_info$milestone_source]]
  } else {
    cell_info$milestone_id <- source_info$milestone_source(cell_info)
  }

  milestone_network <- source_info$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  grouping <- cell_info %>% select(cell_id, milestone_id) %>% filter(milestone_id %in% milestone_ids) %>% deframe()
  cell_info <- cell_info %>% slice(match(names(grouping), cell_id))
  counts <- counts[cell_info$cell_id, ]

  save_raw_dataset(
    lst(milestone_network, cell_info, grouping, counts),
    id
  )
}
