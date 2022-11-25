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
    method_tool_id = case_when(
      !are_na(method_tool_id) ~ method_tool_id,
      grepl("^projected_", method_id) ~ gsub("projected_", "", method_id),
      TRUE ~ method_id
    )
  )

methods$wrapper_trajectory_types <- map(methods$wrapper_trajectory_types, function(x) {
  x[x == "cyclic"] <- "cycle"
  x
})

# add detects_... columns for trajectory types
trajectory_type_ids <- trajectory_types$id
methods_detects <-
  methods$wrapper_trajectory_types %>%
  map(~as.list(set_names(trajectory_type_ids %in% ., trajectory_type_ids))) %>%
  bind_rows() %>%
  rename_all(~paste0("detects_", .))
methods <- methods %>% bind_cols(methods_detects)

# add most complex trajectory type (the latest in dynwrap::trajectory_types)
methods$wrapper_most_complex_trajectory_type <- methods$wrapper_trajectory_types %>% map_chr(~ last(trajectory_type_ids[trajectory_type_ids %in% .]))

# add requires_priors column
methods$wrapper_requires_prior <- map_lgl(methods$wrapper_inputs, ~any(dynwrap::priors$prior_id %in% .$required))
methods$wrapper_required_priors <- map(methods$wrapper_inputs, ~intersect(dynwrap::priors$prior_id, .$required))

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

if (length(setdiff(methods$method_id, methods_google$method_id))) {
  stop(setdiff(methods$method_id, methods_google$method_id))
}

methods <- left_join(
  methods,
  methods_google,
  "method_id"
)

methods$manuscript_publication_date <- as.Date(methods$manuscript_publication_date)
methods$manuscript_preprint_date <- as.Date(methods$manuscript_preprint_date)

#   ____________________________________________________________________________
#   Tools                                                                   ####
tools_google <- sheet %>%
  gs_read(ws = "tools")

tools <- methods %>%
  filter(method_source == "tool") %>%
  rename(tool_id = method_tool_id) %>%
  group_by(tool_id) %>%
  filter(row_number() == 1) %>%
  ungroup()

tools <- tools %>%
  full_join(tools_google, "tool_id")

tools_excluded <- sheet %>%
  gs_read(ws = "tools_excluded", col_types = cols(.default = "c", method_evaluated = "l")) %>%
  filter(!tool_id %in% tools$tool_id) %>%
  mutate(wrapper_trajectory_types = map(wrapper_trajectory_types, str_split, ", ", simplify = TRUE)) %>%
  mutate(wrapper_most_complex_trajectory_type = map_chr(wrapper_trajectory_types, ~ last(trajectory_type_ids[trajectory_type_ids %in% .]))) %>%
  mutate(
    manuscript_publication_date = as.Date(manuscript_publication_date),
    manuscript_preprint_date = as.Date(manuscript_preprint_date)
  )

tools <- bind_rows(tools, tools_excluded)

# Dates ------------------------------
tools$manuscript_date <- tools$manuscript_preprint_date
replace_date <- is.na(tools$manuscript_date) & !is.na(tools$manuscript_publication_date)
tools$manuscript_date[replace_date] <- tools$manuscript_publication_date[replace_date]

# Altmetrics ----------------------------
gsids <- tools$manuscript_google_scholar_cluster_id
cits <- map_int(seq_along(gsids), function(i) {
  gsid <- gsids[[i]]
  cat("Polling ", i, "/", length(gsids), ": ", gsid, sep = "")
  # Sys.sleep(1)
  cits <- google_scholar_num_citations(gsid)
  cat(", number of citations: ", cits, "\n", sep = "")
  as.integer(cits)
})
tools$manuscript_citations <- ifelse(is.na(cits), 0L, cits)
tools_altmetrics <- map(tools$manuscript_doi, function(doi) {
  tryCatch(
    rAltmetric::altmetrics(doi = doi) %>% rAltmetric::altmetric_data() %>% select(cited_by_posts_count) %>% mutate_all(as.numeric),
    error = function(x) tibble(cited_by_posts_count = 0)
  )
}) %>% bind_rows()
tools_altmetrics[is.na(tools_altmetrics)] <- 0
colnames(tools_altmetrics) <- paste0("manuscript_", colnames(tools_altmetrics))
tools <- bind_cols(tools, tools_altmetrics)

methods <- methods %>%
  left_join(tools %>% select(method_tool_id = tool_id, manuscript_bibtex, manuscript_date, manuscript_citations, manuscript_cited_by_posts_count), by = "method_tool_id")

#   ____________________________________________________________________________
#   Save output                                                             ####
write_rds(methods, result_file("methods.rds"), compress = "xz")
write_rds(tools, result_file("tools.rds"), compress = "xz")

