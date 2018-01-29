# This file will process the method characteristics google sheets
library(tidyverse)
library(googlesheets)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

# Downloading -----------------------
# # If it's your first time running this script, run this:
# gs_auth()

method_df <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Software", col_types = cols(GScholarClusterID = "c"), skip = 1)

# Dates ------------------------------
method_df$date <- method_df$Preprint
method_df$date[is.na(method_df$date)] <- method_df$PubDate[is.na(method_df$date)]

# Num citations ---------------------------------------
library(rcrossref)

method_df$ncitations <- pbapply::pbsapply(cl=4, method_df$DOI, cr_citation_count)

# Trajectory components --------------------------
# split maximal trajectory types
method_df <- method_df %>%
  mutate(maximal_trajectory_types_split = map(maximal_trajectory_types, ~as.character(stringr::str_split(., "[ ]?,[ ]?", simplify=TRUE)))) %>%
  mutate(maximal_trajectory_type = map_chr(maximal_trajectory_types_split, first))

# now add for every trajectory type a column, whether it can handle such a trajectory or not
trajectory_types <- read_rds(derived_file("trajectory_types.rds", "dataset_characterisation"))

trajectory_type_capabilities <- map(
  method_df$maximal_trajectory_types_split,
  function(maximal_trajectory_types) {
    ancestors <- trajectory_types %>%
      filter(id %in% maximal_trajectory_types) %>%
      pull(ancestors) %>%
      unlist() %>%
      unique()
    trajectory_types$id %in% ancestors
  }) %>%
  map(~as_tibble(as.list(setNames(., trajectory_types$id)))) %>%
  bind_rows()

method_df <- method_df %>% bind_cols(trajectory_type_capabilities)

## Non inclusion reasons
method_df$non_inclusion_reasons_split <- method_df$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

# Saving -------------------------
write_rds(method_df, derived_file("method_df.rds"))
