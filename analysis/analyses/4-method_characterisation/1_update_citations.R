# This file will update the number of citations in the google sheets
library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment("4-method_characterisation")

# Downloading -----------------------
# # If it's your first time running this script, run this:
# gs_auth()

sheet <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE")

methods <- sheet %>%
  gs_read(ws = "Software", col_types = cols(GScholarClusterID = "c"), skip = 1)

ncitations <- pbapply::pbsapply(methods$GScholarClusterID, dynutils::google_scholar_num_citations)
ncitations[is.na(ncitations)] <- ""

gs_edit_cells(sheet, input = ncitations, byrow = FALSE, anchor = "AK3")
