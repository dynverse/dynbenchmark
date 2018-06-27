# This file will update the number of citations in the google sheets
library(tidyverse)
library(googlesheets)
library(dynbenchmark)

experiment("4-method_characterisation")

# Downloading -----------------------
# # If it's your first time running this script, run this:
# gs_auth()

sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE")

methods <- sheet %>%
  gs_read(ws = "Implementations", col_types = cols(gscholar_cluster_id = "c"), skip = 1)

ncitations <- pbapply::pbsapply(methods$gscholar_cluster_id, google_scholar_num_citations)
# ncitations <- pbapply::pbsapply(methods$doi, rcrossref::cr_citation_count)
ncitations[is.na(ncitations)] <- ""

gs_edit_cells(sheet, ws = "Implementations", input = ncitations, byrow = FALSE, anchor = which(colnames(methods) == "ncitations") %>% {paste0(LETTERS[floor(./26)], LETTERS[. %% 26], "3")})

