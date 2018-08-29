# https://figshare.com/articles/Single-cell_RNA-seq_data_from_Smart-seq2_sequencing_of_FACS_sorted_cells/5715040

library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/tabula-muris")

# cell info
cell_assignments_file <- download_dataset_source_file("annotations_FACS.csv", "https://ndownloader.figshare.com/files/10039267")
all_cell_info <- read_csv(cell_assignments_file) %>%
  mutate(group_id = cell_ontology_class)

counts_zip <- download_dataset_source_file("FACS.zip", "https://ndownloader.figshare.com/files/10038307")
all_cell_info <- read_csv(cell_assignments_file) %>%
  mutate(group_id = cell_ontology_class)
