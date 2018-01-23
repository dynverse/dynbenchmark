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

# Trajectory components --------------------------
method_df <- method_df %>%
  mutate(maximal_trajectory_type = factor(maximal_trajectory_type, levels = c("undirected_linear", "simple_fork", "complex_fork", "unrooted_tree", "undirected_graph", "disconnected_undirected_graph"))) %>%
  mutate(
    disconnected_undirected_graph = as.numeric(maximal_trajectory_type) >= 6 & !grepl("disconnected_undirected_graph", unable_trajectory_type),
    undirected_graph = as.numeric(maximal_trajectory_type) >= 5 & !grepl("undirected_graph", unable_trajectory_type),
    undirected_cycle = as.numeric(maximal_trajectory_type) >= 5 & !grepl("undirected_graph", unable_trajectory_type),
    unrooted_tree = as.numeric(maximal_trajectory_type) >= 4 & !grepl("unrooted_tree", unable_trajectory_type),
    complex_fork = as.numeric(maximal_trajectory_type) >= 3 & !grepl("complex_fork", unable_trajectory_type),
    simple_fork = as.numeric(maximal_trajectory_type) >= 2 & !grepl("simple_fork", unable_trajectory_type),
    undirected_linear = as.numeric(maximal_trajectory_type) >= 1 & !grepl("undirected_linear", unable_trajectory_type)
  )

# source("analysis/analyses/4-method_characterization/0_common.R")
# method_df_evaluated <- method_df_evaluated %>%
#   gather("trajectory_type", "can_trajectory_type", !!trajectory_types) %>%
#   group_by(name) %>%
#   filter(can_trajectory_type) %>%
#   filter(row_number() == n()) %>%
#   select(name, trajectory_type) %>%
#   ungroup() %>%
#   right_join(method_df_evaluated, by = "name")

# Saving -------------------------
write_rds(method_df, derived_file("method_df.rds"))
