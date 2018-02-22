# This file will process the implementation characteristics google sheets
library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("4-method_characterisation")

# Downloading -----------------------
# # If it's your first time running this script, run this:
# gs_auth()

implementations <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Software", col_types = cols(GScholarClusterID = "c"), skip = 1)

# Dates ------------------------------
implementations$date <- implementations$Preprint
implementations$date[is.na(implementations$date)] <- implementations$PubDate[is.na(implementations$date)]

# Trajectory components --------------------------
# split maximal trajectory types
implementations <- implementations %>%
  mutate(maximal_trajectory_types_split = map(maximal_trajectory_types, ~as.character(stringr::str_split(., "[ ]?,[ ]?", simplify=TRUE)))) %>%
  mutate(maximal_trajectory_type = map_chr(maximal_trajectory_types_split, first))

# now add for every trajectory type a column, whether it can handle such a trajectory or not
trajectory_type_capabilities <- map(
  implementations$maximal_trajectory_types_split,
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

implementations <- implementations %>% bind_cols(trajectory_type_capabilities)

## Non inclusion reasons ------------------------
implementations$non_inclusion_reasons_split <- implementations$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

## implementation conversions ------------------------
implementations$conversion_split <- implementations$conversion %>% str_split("[ ]?,[ ]?")
allowed_conversions <- c("branching_local", "linear", "branching_cluster", "branching_global", "branching_local_projection", "special", NA)
if(!implementations$conversion_split %>% map_lgl(~all(. %in% allowed_conversions)) %>% all()) {
  stop("Some conversions are invalid!")
}
implementations$conversion_special <- map_lgl(implementations$conversion_split, ~"special" %in% .)

## implementation outputs --------------
implementations$output_split <- implementations$output %>% str_split("[ ]?,[ ]?")

# add extra reason for date cutoff
date_cutoff <- as.Date("2017-06-01")
date_filter <- implementations$date > date_cutoff
implementations$non_inclusion_reasons_split[date_filter] <- map(implementations$non_inclusion_reasons_split[date_filter], c, "date") %>% map(unique)

## Infer whether the structure is fixed based on other columns -------------
structure_fix <- function(maximal_trajectory_type, n_branches, n_end_states, ...) {
  if (any(is.na(c(maximal_trajectory_type, n_branches, n_end_states)))) {
    NA
  } else if(maximal_trajectory_type == "undirected_linear") {
    "algorithm"
  } else if (n_branches == "required" || n_branches == "required_default" || n_end_states == "required" || n_end_states == "required_default") {
    "parameter"
  } else {
    "free"
  }
}
implementations$fixes_structure <- pmap_chr(implementations, structure_fix)

# Saving -------------------------
write_rds(implementations, derived_file("implementations.rds"))
