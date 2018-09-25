library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/silver/epiblast_nakamura")

rpm_ms_location <- download_dataset_source_file(
  "GSE74767_SC3seq_Ms_ProcessedData.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE74767&format=file&file=GSE74767%5FSC3seq%5FMs%5FProcessedData%2Etxt%2Egz"
)
rpm_cy_location <- download_dataset_source_file(
  "GSE74767_SC3seq_Cy_ProcessedData.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE74767&format=file&file=GSE74767%5FSC3seq%5FCy%5FProcessedData%2Etxt%2Egz"
)

rpm_ms <- read_tsv(rpm_ms_location) %>% as.data.frame() %>% .[, -2] %>% column_to_rownames("mm10_entrez_id") %>% as.matrix() %>% t
rpm_cy <- read_tsv(rpm_cy_location) %>%
  .[, -2] %>%
  group_by(macFas5_entrez_id) %>%
  summarise_all(max) %>%
  as.data.frame() %>%
  column_to_rownames("macFas5_entrez_id") %>%
  as.matrix() %>%
  t()

geo <- GEOquery::getGEO("GSE74767", destdir = dataset_source_file(""))

celldata_location <- download_dataset_source_file(
  "nature19096-s1.xlsx",
  "https://media.nature.com/original/nature-assets/nature/journal/v537/n7618/extref/nature19096-s1.xlsx"
)

allcell_info <- readxl::read_excel(celldata_location, "SC3-seq_SampleTable", skip = 1) %>%
  rename(cell_id = SampleID) %>%
  mutate(Group = ifelse(Group %in% c("cyESCoF", "cyESCFF"), "cyESC", Group))


settings <- list(
  list(
    id = "real/silver/epiblast-monkey_nakamura",
    milestone_network = tribble(
      ~from, ~to,
      "ICM", "Pre-EPI",
      "Pre-EPI", "PostE-Epi",
      "Pre-EPI", "cyESC",
      "Pre-EPI", "Gast1",
      "PostE-Epi", "PostL-EPI",
      "PostE-Epi", "Gast2a",
      "Gast2a", "Gast2b"
    ),
    milestone_source = "Group",
    rpm = rpm_cy
  ),
  list(
    id = "real/silver/trophectoderm-monkey_nakamura",
    milestone_network = tribble(
      ~from, ~to,
      "PreE-TE", "PreL-TE",
      "PreL-TE", "Post-paTE"
    ),
    milestone_source = "Group",
    rpm = rpm_cy
  ),
  list(
    id = "real/silver/ICM-monkey_nakamura",
    milestone_network = tribble(
      ~from, ~to,
      "ICM", "Hypoblast",
      "Hypoblast", "EXMC",
      "Hypoblast", "VE/YE",
      "ICM", "Pre-EPI",
      "Pre-EPI", "PostE-Epi",
      "Pre-EPI", "cyESC",
      "Pre-EPI", "Gast1",
      "PostE-Epi", "PostL-EPI",
      "PostE-Epi", "Gast2a",
      "Gast2a", "Gast2b"
    ),
    milestone_source = "Group",
    rpm = rpm_cy
  ),
  list(
    id = "real/silver/blastocyst-monkey_nakamura",
    milestone_network = tribble(
      ~from, ~to,
      "ICM", "Hypoblast",
      "Hypoblast", "EXMC",
      "Hypoblast", "VE/YE",
      "ICM", "Pre-EPI",
      "Pre-EPI", "PostE-Epi",
      "Pre-EPI", "cyESC",
      "Pre-EPI", "Gast1",
      "PostE-Epi", "PostL-EPI",
      "PostE-Epi", "Gast2a",
      "Gast2a", "Gast2b",
      "PreE-TE", "PreL-TE",
      "PreL-TE", "Post-paTE"
    ),
    milestone_source = "Group",
    rpm = rpm_cy
  )
) %>% lapply(function(l) {
  l$milestone_network <- l$milestone_network %>% mutate(length = 1, directed = TRUE)
  l
})


# assuming that 1 is always the highest expression in each cell (next to 0)
rpm_to_counts <- function(rpm) {
  extract_counts <- function(x) {
    tab <- table(x[x>0])
    norm <- which.max(tab) %>% names() %>% as.numeric()
    x <- round(x / norm)
  }

  rpm %>% apply(1,extract_counts) %>% t
}


for (setting in settings) {
  allcounts <- rpm_to_counts(setting$rpm)
  allcounts[is.na(allcounts)] <- 0

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}

