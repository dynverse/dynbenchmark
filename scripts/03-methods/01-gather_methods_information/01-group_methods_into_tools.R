#' Grouping methods into tools
#' (1) Grouping of methods into "tools", based on the google spreadsheet
#' (2) Some postprocessing of the dynmethods::methods

library(tidyverse)
library(googlesheets)
library(dynbenchmark)

experiment("03-methods")

# If it's your first time running this script, run this:
# gs_auth()
sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE")

##  ............................................................................
##  Methods                                                                 ####
methods <- dynmethods::methods

# tool_id is default the method_id
methods$tool_id <- ifelse(is.na(methods$implementation_id), methods$id, methods$implementation_id)

# add detects_... columns for trajectory types
trajectory_type_ids <- trajectory_types$id
methods_detects <- methods$trajectory_types %>%
  map(~as.list(set_names(trajectory_type_ids %in% ., trajectory_type_ids))) %>%
  bind_rows() %>%
  rename_all(~paste0("detects_", .))
methods <- methods %>% bind_cols(methods_detects)

# add most complex trajectory type (the latest in dynwrap::trajectory_types)
methods$most_complex_trajectory_type <- methods$trajectory_types %>% map_chr(~ last(trajectory_type_ids[trajectory_type_ids %in% .]))

# join with google sheet
methods_google <- sheet %>%
  gs_read(ws = "methods")

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
#   Tools                                                                   ####
tools_google <- sheet %>%
  gs_read(ws = "tools")

tools <- methods %>%
  filter(type == "algorithm") %>%
  group_by(tool_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

tools <- tools %>%
  full_join(tools_google, "tool_id")

tools_excluded <- sheet %>%
  gs_read(ws = "tools_excluded") %>%
  filter(!tool_id %in% tools$tool_id) %>%
  mutate(trajectory_types = map(trajectory_types, str_split, ", ", simplify = FALSE))

tools <- bind_rows(tools, tools_excluded)

# Dates ------------------------------
tools$date <- tools$preprint_date
replace_date <- is.na(tools$date) & !is.na(tools$publication_date)
tools$date[replace_date] <- tools$publication_date[replace_date]

# Altmetrics ----------------------------
tools_altmetrics <- map(tools$doi, function(doi) {
  tryCatch(
    rAltmetric::altmetrics(doi = doi) %>% rAltmetric::altmetric_data() % >% select(ends_with("_count")) %>% mutate_all(as.numeric),
    error = function(x) tibble(cited_by_posts_count = 0)
  )
}) %>% bind_rows()
tools_altmetrics[is.na(tools_altmetrics)] <- 0
tools <- bind_cols(tools, tools_altmetrics)

#   ____________________________________________________________________________
#   Save output                                                             ####
write_rds(methods, result_file("methods.rds"), compress = "xz")
write_rds(tools, result_file("tools.rds"), compress = "xz")

