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

# --------------------
method_df_evaluated <- method_df %>%
  filter(wrapper == "Done")

# Trajectory components --------------------------
method_df_evaluated <- method_df_evaluated %>%
  mutate(
    segment = stringr::str_detect(trajectory_components, "segment"),
    fork = stringr::str_detect(trajectory_components, "fork"),
    convergence = stringr::str_detect(trajectory_components, "convergence"),
    loop = stringr::str_detect(trajectory_components, "loop"),
    n_forks = as.numeric(ifelse(fork, gsub(".*fork\\(.*,([0-9]|inf)\\).*", "\\1", trajectory_components), NA)),
    n_fork_paths = as.numeric(ifelse(fork, gsub(".*fork\\(([0-9]|inf),.*", "\\1", trajectory_components), NA)),
    n_convergences = as.numeric(ifelse(convergence, gsub(".*convergence\\(.*,([0-9]|inf)\\).*", "\\1", trajectory_components), NA)),
    n_convergence_paths = as.numeric(ifelse(convergence, gsub(".*convergence\\(([0-9]|inf),.*", "\\1", trajectory_components), NA))
  ) %>%
  mutate(
    undirected_linear = segment,
    simple_fork = fork,
    unrooted_tree = fork & n_forks > 1,
    complex_fork = fork & n_fork_paths > 2,
    undirected_cycle = loop,
    undirected_graph = unrooted_tree & complex_fork & convergence
  )

source("analysis/analyses/4-method_characterization/0_common.R")
method_df_evaluated <- method_df_evaluated %>%
  gather("trajectory_type", "can_trajectory_type", !!trajectory_types) %>%
  group_by(name) %>%
  filter(can_trajectory_type) %>%
  filter(row_number() == n()) %>%
  select(name, trajectory_type) %>%
  ungroup() %>%
  right_join(method_df_evaluated, by = "name")

# Saving -------------------------
write_rds(method_df, derived_file("method_df.rds"))
write_rds(method_df_evaluated, derived_file("method_df_evaluated.rds"))
