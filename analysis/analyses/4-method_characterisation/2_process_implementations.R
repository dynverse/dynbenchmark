# This file will process the implementation en method characteristics google sheets
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
  gs_read(ws = "Implementations", col_types = cols(gscholar_cluster_id = "c"), skip = 1) %>%
  filter(contains_ti)

# Dates ------------------------------
implementations$date <- implementations$preprint_date
replace_date <- is.na(implementations$date) & !is.na(implementations$publication_date)
implementations$date[replace_date] <- implementations$publication_date[replace_date]

# Altmetrics ----------------------------
implementations_altmetrics <- map(implementations$DOI, function(doi) {
  tryCatch(
    rAltmetric::altmetrics(doi = doi) %>% rAltmetric::altmetric_data() %>% select(ends_with("_count")) %>% mutate_all(as.numeric),
    error = function(x) tibble(cited_by_posts_count = 0)
  )
}) %>% bind_rows()
implementations_altmetrics[is.na(implementations_altmetrics)] <- 0
implementations <- bind_cols(implementations, implementations_altmetrics)

## Non inclusion reasons ------------------------
implementations$non_inclusion_reasons_split <- implementations$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

## Platforms ------------------
implementations$platforms_split <- implementations$platforms %>% str_split("[ ]?,[ ]?")

#   ____________________________________________________________________________
#   Methods                                                                 ####
methods <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Methods", skip = 1)

# Trajectory components --------------------------
# split maximal trajectory types
methods <- methods %>%
  mutate(maximal_trajectory_types_split = map(maximal_trajectory_types, ~as.character(stringr::str_split(., "[ ]?,[ ]?", simplify = TRUE)))) %>%
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

## Methods conversions ------------------------
methods$conversion_split <- methods$conversion %>% str_split("[ ]?,[ ]?")
allowed_conversions <- c("trajectory", "linear", "end_state_probability", "dimred_projection", "cluster_graph", "cell_graph", "special", "cyclic", NA)
if(!methods$conversion_split %>% map_lgl(~all(. %in% allowed_conversions)) %>% all()) {
  stop("Some conversions are invalid!")
}
methods$conversion_special <- map_lgl(methods$conversion_split, ~"special" %in% .)

## Prior information ---------------------------
methods$prior_start <- case_when(
  methods$start_id == "required" ~ "required_id",
  methods$start_id == "optional" ~ "optional_id"
)
methods$prior_end <- case_when(
  methods$end_id == "required" ~ "required_id",
  methods$end_id == "optional" ~ "optional_id",
  methods$end_n == "required" ~ "required_#",
  methods$end_n == "optional" ~ "optional_#"
)
methods$prior_states <- case_when(
  methods$states_id == "required" ~ "required_id",
  methods$states_id == "optional" ~ "optional_id",
  methods$states_n == "required" ~ "required_#",
  methods$states_n == "optional" ~ "optional_#",
  methods$states_network == "required" ~ "required_network",
  methods$states_network == "optional" ~ "optional_network"
)
methods$prior_features <- case_when(
  methods$features_id == "required" ~ "required_id",
  methods$features_id == "optional" ~ "optional_id"
)

## Dynmethods information ------------------------
methods_dynmethods <- dynmethods::methods
methods <- methods %>% left_join(
  methods_dynmethods[setdiff(colnames(methods_dynmethods), c(colnames(methods), colnames(implementations))) %>% c("method_id")],
  "method_id"
)

#   ____________________________________________________________________________
#   Save output                                                             ####
write_rds(methods, derived_file("methods_tidy.rds"))
write_rds(implementations, derived_file("implementations_tidy.rds"))

