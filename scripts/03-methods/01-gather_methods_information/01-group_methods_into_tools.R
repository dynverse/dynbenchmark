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

are_na <- function(x) map_lgl(x, is.na)
methods <- dynmethods::methods %>%
  mutate(
    tool_id = case_when(
      !are_na(implementation_id) ~ implementation_id,
      grepl("^projected_", id) ~ gsub("projected_", "", id),
      TRUE ~ id
    )
  )

# add detects_... columns for trajectory types
trajectory_type_ids <- trajectory_types$id
methods_detects <- methods$trajectory_types %>%
  map(~as.list(set_names(trajectory_type_ids %in% ., trajectory_type_ids))) %>%
  bind_rows() %>%
  rename_all(~paste0("detects_", .))
methods <- methods %>% bind_cols(methods_detects)

# add most complex trajectory type (the latest in dynwrap::trajectory_types)
methods$most_complex_trajectory_type <- methods$trajectory_types %>% map_chr(~ last(trajectory_type_ids[trajectory_type_ids %in% .]))

# add requires_priors column
methods$requires_prior <- map_lgl(methods$input, ~any(dynwrap::priors$prior_id %in% .$required))
methods$required_priors <- map(methods$input, ~intersect(dynwrap::priors$prior_id, .$required))

# TEMPORARY fix for wrapper types, awaiting updated dynmethods
wrapper_type_map <- c(
  linear_trajectory = "linear",
  cyclic_trajectory = "cyclic",
  trajectory = "direct",
  cell_graph = "cell_graph",
  cluster_graph = "cluster_assignment",
  control = NA,
  dimred_projection = "orth_proj",
  end_state_probabilities = "end_state_prob",
  branch_trajectory = "direct"
)
methods$wrapper_type <- wrapper_type_map[methods$wrapper_type]
testthat::expect_true(all((methods$wrapper_type %in% dynwrap::wrapper_types$id) | is.na(methods$wrapper_type)))

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
  filter(source == "tool") %>%
  group_by(tool_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

tools <- tools %>%
  full_join(tools_google, "tool_id")

tools_excluded <- sheet %>%
  gs_read(ws = "tools_excluded") %>%
  filter(!tool_id %in% tools$tool_id) %>%
  mutate(trajectory_types = map(trajectory_types, str_split, ", ", simplify = TRUE)) %>%
  mutate(most_complex_trajectory_type = map_chr(trajectory_types, ~ last(trajectory_type_ids[trajectory_type_ids %in% .]))) %>%
  mutate(
    publication_date = as.Date(publication_date),
    preprint_date = as.Date(preprint_date),
    date = ifelse(is.na(publication_date), preprint_date, publication_date)
  )

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

