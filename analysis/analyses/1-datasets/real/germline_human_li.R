rm(list=ls())
library(tidyverse)
library(dynalysis)

dataset_preprocessing("real/germline_human_li")

tar_location <- download_dataset_file(
  "GSE86146_RAW.tar",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE86146&format=file"
)

utils::untar(tar_location, exdir = dataset_preproc_file(""))

allcounts <- list.files(dataset_preproc_file("")) %>%
  keep(~ startsWith(., "GSM")) %>%
  map(~ read_tsv(dataset_preproc_file(.), col_types = cols(.default = "d", Gene = "c")) %>% gather(Sample, Expression, -Gene)) %>%
  bind_rows() %>%
  spread(Gene, Expression) %>%
  as.data.frame() %>%
  magrittr::set_rownames(., .$Sample) %>%
  select(-Sample) %>%
  as.matrix

mmc2_location <- download_dataset_file(
  "mmc2.xlsx",
  "http://www.sciencedirect.com/science/MiamiMultiMediaURL/1-s2.0-S1934590917300784/1-s2.0-S1934590917300784-mmc2.xlsx/274143/html/S1934590917300784/139b580f8ca22e965c646ac00b373e93/mmc2.xlsx"
)

allcell_info <- readxl::read_xlsx(mmc2_location, sheet = 3) %>%
  rename(cell_id = Cell, cluster=Cluster) %>%
  filter(cell_id %in% rownames(allcounts)) %>%
  mutate(
    week = as.numeric(gsub("[FM]_(.+)W_.*", "\\1", cell_id)),
    gender = gsub("([FM]).*", "\\1", cell_id),
    type = gsub(".*_(Soma|FGC).*", "\\1", cluster),
    weekgendertype = paste0(gender, "#", week, "#", type)
  )

milestone_order_to_network <- function(order) {
  tibble(from = order[-length(order)], to = order[-1])
}

settings <- list(
  list(
    id = "real/germline_human_male_li",
    milestone_network = tribble(
      ~from, ~to,
      "Male_FGC#1", "Male_FGC#2",
      "Male_FGC#2", "Male_FGC#3"
    ),
    milestone_source = "cluster",
    ti_type = "linear"
  ),
  list(
    id = "real/germline_human_female_li",
    milestone_network = tribble(
      ~from, ~to,
      "Female_FGC#1", "Female_FGC#2",
      "Female_FGC#2", "Female_FGC#3"
    ),
    milestone_source = "cluster",
    ti_type = "linear"
  ),
  list(
    id = "real/germline_human_male_weeks_li",
    milestone_network = tribble(
      ~from, ~to,
      "M#4#FGC", "M#9#FGC",
      "M#9#FGC", "M#10#FGC",
      "M#10#FGC", "M#19#FGC",
      "M#19#FGC", "M#20#FGC",
      "M#20#FGC", "M#21#FGC",
      "M#21#FGC", "M#25#FGC"
    ),
    milestone_source = "weekgendertype",
    ti_type = "linear"
  ),
  list(
    id = "real/germline_human_female_weeks_li",
    milestone_network = tribble(
      ~from, ~to,
      "F#5#FGC", "F#7#FGC",
      "F#7#FGC", "F#8#FGC",
      "F#8#FGC", "F#10#FGC",
      "F#10#FGC", "F#11#FGC",
      "F#11#FGC", "F#12#FGC",
      "F#12#FGC", "F#14#FGC",
      "F#14#FGC", "F#18#FGC",
      "F#18#FGC", "F#20#FGC",
      "F#20#FGC", "F#23#FGC",
      "F#23#FGC", "F#24#FGC",
      "F#24#FGC", "F#26#FGC"
    ),
    milestone_source = "weekgendertype",
    ti_type = "linear"
  )
) %>% lapply(function(l) {
  l$milestone_network <- l$milestone_network %>% mutate(length = 1, directed = TRUE)
  l
})

for (setting in settings) {
  dataset_preprocessing(setting$id)

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

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

