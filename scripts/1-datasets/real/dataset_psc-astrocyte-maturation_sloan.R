library(tidyverse)
library(dynverse)

dataset_preprocessing("real/pancreatic-cell-maturation_zhang")

txt_location <- download_dataset_file(
  "GSE99951_all_data_htseq_out.csv",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE99951&format=file&file=GSE99951%5Fall%5Fdata%5Fhtseq%5Fout%2Ecsv%2Egz"
)

# geo <- GEOquery::getGEO("GSE99951", destdir = dataset_preproc_file(""))
# cell_info_all <- geo[[1]] %>%
#   Biobase::phenoData() %>%
#   as("data.frame") %>%
#   rename(cell_id=title, milestone_id=characteristics_ch1.3, group=characteristics_ch1.5) %>%
#   mutate(cell_id=gsub("([^ ]*).*", "X\\1", cell_id)) %>%
#   mutate_all(funs(as.character))


# temporary workaround because the above does not seem to be working
geo <- GEOquery::getGEO("GSE99951", destdir = dataset_preproc_file(""), GSElimits = c(1,1), GSEMatrix = FALSE, getGPL = FALSE, AnnotGPL = FALSE)


cell_info_all <- dynutils::list_as_tibble(geo@gsms %>% map(~.@header)) %>%
  rowwise() %>%
  filter(length(characteristics_ch1) >= 7) %>%
  mutate(
    cell_id = gsub("([^ ]*).*", "X\\1", title),
    milestone_id = characteristics_ch1[[4]],
    group = characteristics_ch1[[6]]
  ) %>%
  ungroup() %>%
  select(cell_id, milestone_id, group) %>%
  mutate_all(funs(as.character))
# end temporary workaround

counts_all <- read.table(txt_location, TRUE, " ", stringsAsFactors = FALSE) %>% as.matrix() %>% t


settings <- list(
  list(
    id = "real/psc-astrocyte-maturation-neuron_sloan",
    group_id = "cell type: neuron"
  ),
  list(
    id = "real/psc-astrocyte-maturation-glia_sloan",
    group_id = "cell type: glia"
  )
)

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "age: Day 100", "age: Day 130", 30, TRUE,
  "age: Day 130", "age: Day 175", 45, TRUE,
  "age: Day 175", "age: Day 450", 275, TRUE
)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

for (setting in settings) {
  dataset_preprocessing(setting$id)

  cell_info <- cell_info_all %>% slice(match(rownames(counts_all), cell_id)) %>%
    filter(group == setting$group_id, milestone_id %in% milestone_ids)

  counts <- counts_all[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  preprocess_dataset(
    counts = counts,
    milestone_network = milestone_network,
    grouping = grouping,
    cell_info = cell_info
  )
}

