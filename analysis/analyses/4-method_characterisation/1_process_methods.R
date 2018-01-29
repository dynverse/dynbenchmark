# This file will process the method characteristics google sheets

source("analysis/analyses/4-method_characterization/0_common.R")
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
script_file <- "analysis/analyses/4-method_characterization/scholar.py"
if (!file.exists(script_file)) {
  download.file("https://raw.githubusercontent.com/ckreibich/scholar.py/master/scholar.py", destfile = script_file)
}

num_citations_by_clusterid <- function(clusterid, scholar_file = script_file) {
  tryCatch({
    command <- paste0("python ", scholar_file, " -C ", clusterid, " --csv-header")
    output <- system(command, intern = T)
    tab <- readr::read_delim(paste(gsub("\n", " ", output), collapse = "\n"), delim = "|")
    sum(tab$num_citations)
  }, error = function(e) NA)
}

method_df <- method_df %>%
  mutate(citations = pbapply::pbsapply(cl=4, GScholarClusterID, num_citations_by_clusterid))

# Trajectory components --------------------------
# split maximal trajectory types
method_df <- method_df %>%
  mutate(maximal_trajectory_types_split = map(maximal_trajectory_types, ~stringr::str_split(., "[ ]?,[ ]?", simplify=TRUE))) %>%
  mutate(maximal_trajectory_type = map_chr(maximal_trajectory_types_split, first))

# now add for every trajectory type a column, whether it can handle such a trajectory or not
trajectory_type_capabilities <- map(
  method_df$maximal_trajectory_types_split,
  function(maximal_trajectory_types) {
    trajectory_types %in%
      unique(unlist(trajectory_type_ancestors[maximal_trajectory_types]))
  }) %>%
  map(~as_tibble(as.list(setNames(., trajectory_types)))) %>%
  bind_rows()

method_df <- method_df %>% bind_cols(trajectory_type_capabilities)

## Non inclusion reasons
method_df$non_inclusion_reasons_split <- method_df$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

# Saving -------------------------
write_rds(method_df, derived_file("method_df.rds"))
