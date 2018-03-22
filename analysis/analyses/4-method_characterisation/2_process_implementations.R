# This file will process the implementation characteristics google sheets
library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("4-method_characterisation")


#   ____________________________________________________________________________
#   Implementations                                                         ####

# Downloading -----------------------
# # If it's your first time running this script, run this:
# gs_auth()

implementations <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Implementations", col_types = cols(GScholarClusterID = "c"), skip = 1) %>%
  filter(contains_ti)

# Dates ------------------------------
implementations$date <- implementations$Preprint
replace_date <- is.na(implementations$date) & !is.na(implementations$PubDate)
implementations$date[replace_date] <- implementations$PubDate[replace_date]

# Altmetrics ----------------------------
implementations_altmetrics <- map(implementations$DOI, function(doi) {
  tryCatch(
    rAltmetric::altmetrics(doi=doi) %>% rAltmetric::altmetric_data() %>% select(ends_with("_count")) %>% mutate_all(as.numeric),
    error = function(x) tibble(cited_by_posts_count=0)
  )
}) %>% bind_rows()
implementations_altmetrics[is.na(implementations_altmetrics)] <- 0
implementations <- bind_cols(implementations, implementations_altmetrics)


#   ____________________________________________________________________________
#   Methods                                                                 ####

methods <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Methods", skip = 1)

# Trajectory components --------------------------
# split maximal trajectory types
methods <- methods %>%
  mutate(maximal_trajectory_types_split = map(maximal_trajectory_types, ~as.character(stringr::str_split(., "[ ]?,[ ]?", simplify=TRUE)))) %>%
  mutate(maximal_trajectory_type = map_chr(maximal_trajectory_types_split, first))

# now add for every trajectory type a column, whether it can handle such a trajectory or not
trajectory_type_capabilities <- map(
  methods$maximal_trajectory_types_split,
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

# process unhandable trajectory types
trajectory_type_disabilities <- methods$unhandable_trajectory_types %>%
  str_split("[ ]?,[ ]?") %>%
  map(~colnames(trajectory_type_capabilities) %in% .) %>%
  map(~as_tibble(as.list(setNames(., trajectory_types$id)))) %>%
  bind_rows()

trajectory_type_capabilities <- (trajectory_type_capabilities & !trajectory_type_disabilities) %>% as_tibble()

# add capabilities to implementations
methods <- methods %>% bind_cols(trajectory_type_capabilities)

## Non inclusion reasons ------------------------
methods$non_inclusion_reasons_split <- methods$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

## methods conversions ------------------------
methods$conversion_split <- methods$conversion %>% str_split("[ ]?,[ ]?")
allowed_conversions <- c("trajectory", "linear", "end_state_probability", "dimred_projection", "cluster_graph", "cell_graph", "special", NA)
if(!methods$conversion_split %>% map_lgl(~all(. %in% allowed_conversions)) %>% all()) {
  stop("Some conversions are invalid!")
}
methods$conversion_special <- map_lgl(methods$conversion_split, ~"special" %in% .)

## methods outputs --------------
methods$output_split <- methods$output %>% str_split("[ ]?,[ ]?")

# # add extra reason for date cutoff
# date_cutoff <- as.Date("2017-06-01")
# date_filter <- implementations$date > date_cutoff & !is.na(implementations$date)
# implementations$non_inclusion_reasons_split[date_filter] <- map(implementations$non_inclusion_reasons_split[date_filter], c, "date") %>% map(unique)

## Infer whether the topology is fixed based on other columns -------------
topology_fix <- function(maximal_trajectory_type, n_branches, n_end_states, fixes_topology_, ...) {
  if (any(is.na(c(maximal_trajectory_type, n_branches, n_end_states)))) {
    NA
  } else if(maximal_trajectory_type == "directed_linear" | maximal_trajectory_type == "directed_cycle") {
    "algorithm"
  } else if (n_branches == "required" || n_branches == "required_default" || n_end_states == "required" || n_end_states == "required_default") {
    "parameter"
  } else {
    "free"
  }
}
methods$fixes_topology <- pmap_chr(methods, topology_fix)

# Saving -------------------------
write_rds(methods, derived_file("methods_tidy.rds"))
write_rds(implementations, derived_file("implementations.rds"))
