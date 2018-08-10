## Grouping methods into tools
## (1) Grouping of methods into "tools", based on the google spreadsheet
## (2) Some postprocessing of the dynmethods::methods

library(tidyverse)
library(googlesheets)
library(dynbenchmark)

experiment("03-method_characterisation")

# If it's your first time running this script, run this:
# gs_auth()
sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE")

##  ............................................................................
##  Methods                                                                 ####
methods <- dynmethods::methods

# tool_id is default id
methods$tool_id <- ifelse(is.na(methods$implementation_id), methods$id, methods$implementation_id)

# join with google sheet
methods_google <- sheet %>%
  gs_read(ws = "methods", skip = 1)

if (length(setdiff(methods$id, methods_google$id))) {
  stop(setdiff(methods$id, methods_google$id))
}

methods <- left_join(
  methods,
  methods_google,
  c("id")
)

methods$publication_date <- as.Date(methods$publication_date)
methods$preprint_date <- as.Date(methods$preprint_date)

#   ____________________________________________________________________________
#   Implementations                                                         ####
tools_google <- sheet %>%
  gs_read(ws = "tools", skip = 1) %>%
  filter(contains_ti)

tools <- methods %>%
  filter(type == "algorithm") %>%
  group_by(tool_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

tools <- tools %>%
  full_join(tools_google, "tool_id")

# Dates ------------------------------
tools$date <- tools$preprint_date
replace_date <- is.na(tools$date) & !is.na(tools$publication_date)
tools$date[replace_date] <- tools$publication_date[replace_date]

# Altmetrics ----------------------------
tools_altmetrics <- map(tools$doi, function(doi) {
  tryCatch(
    rAltmetric::altmetrics(doi = doi) %>% rAltmetric::altmetric_data() %>% select(ends_with("_count")) %>% mutate_all(as.numeric),
    error = function(x) tibble(cited_by_posts_count = 0)
  )
}) %>% bind_rows()
tools_altmetrics[is.na(tools_altmetrics)] <- 0
tools <- bind_cols(tools, tools_altmetrics)

## Non inclusion reasons ------------------------
tools$non_inclusion_reasons_split <- tools$non_inclusion_reasons %>% str_split("[ ]?,[ ]?")

## Platforms ------------------
tools$platforms_split <- tools$platforms %>% str_split("[ ]?,[ ]?")

#   ____________________________________________________________________________
#   Save output                                                             ####
write_rds(methods, derived_file("methods.rds"))
write_rds(tools, derived_file("tools.rds"))

