# This file will process the implementation en method characteristics google sheets
library(tidyverse)
library(googlesheets)
library(dynverse)
library(dynmethods)

experiment("4-method_characterisation")

# If it's your first time running this script, run this:
# gs_auth()

##  ............................................................................
##  Methods                                                                 ####

data("methods", package = "dynmethods")

# implementation_id is default method_id
methods$implementation_id <- ifelse(is.na(methods$implementation_id), methods$method_id, methods$implementation_id)

# join with google sheet
methods_google <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Methods", skip = 1)

if (length(setdiff(methods$method_id, methods_google$method_id))) {
  stop(setdiff(methods$method_id, methods_google$method_id))
}

methods <- left_join(
  methods,
  methods_google,
  c("method_id")
)

# ## Prior information ---------------------------
data(priors, package = "dynwrap")
methods_priors <- map_dfc(
  priors$prior_id, function(prior_id) {
    tibble(
      !!prior_id :=
        map_lgl(methods$inputs, function(inputs) {
          prior_id %in% inputs$input_id
        })
    )
  })
methods <- methods %>% bind_cols(methods_priors)

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
methods$prior_group <- case_when(
  methods$groups_id == "required" ~ "required_id",
  methods$groups_id == "optional" ~ "optional_id",
  methods$groups_n == "required" ~ "required_#",
  methods$groups_n == "optional" ~ "optional_#",
  methods$groups_network == "required" ~ "required_network",
  methods$groups_network == "optional" ~ "optional_network"
)
methods$prior_features <- case_when(
  methods$features_id == "required" ~ "required_id",
  methods$features_id == "optional" ~ "optional_id"
)

#   ____________________________________________________________________________
#   Implementations                                                         ####
implementations_google <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Implementations", col_types = cols(gscholar_cluster_id = "c"), skip = 1) %>%
  filter(contains_ti)

implementations <- methods %>%
  filter(type == "algorithm") %>%
  group_by(implementation_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

implementations <- implementations %>%
  left_join(implementations_google, "implementation_id")

# Dates ------------------------------
implementations$date <- implementations$preprint_date
replace_date <- is.na(implementations$date) & !is.na(implementations$publication_date)
implementations$date[replace_date] <- implementations$publication_date[replace_date]

# Altmetrics ----------------------------
implementations_altmetrics <- map(implementations$doi, function(doi) {
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
#   Add implementation information to methods                               ####
methods <- left_join(
  methods,
  implementations[, c("implementation_id", setdiff(colnames(implementations), colnames(methods)))],
  "implementation_id"
)

#   ____________________________________________________________________________
#   Save output                                                             ####
write_rds(methods, derived_file("methods_tidy.rds"))
write_rds(implementations, derived_file("implementations_tidy.rds"))

