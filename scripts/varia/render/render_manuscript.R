#' Renders the manuscript

library(dynbenchmark)
library(tidyverse)

# download from google docs
httr::set_config(httr::config(http_version = 0)) # avoid http2 framing layer bug
drive <- googledrive::drive_download(googledrive::as_id("1je6AaelApu2xcSNbYlvcuTzUeUJBOpTUPHz0L9Houfw"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character
system(pritt("cat {drive$local_path} > manuscript/paper.Rmd"))

# render
rmarkdown::render("manuscript/paper.Rmd", output_format = pdf_manuscript())
rmarkdown::render("manuscript/supplementary.Rmd", output_format = pdf_manuscript(render_changes = FALSE))

# browse
system("/usr/bin/xdg-open manuscript/paper.pdf")
system("/usr/bin/xdg-open manuscript/paper_changes.pdf")
system("/usr/bin/xdg-open manuscript/supplementary.pdf")

# upload to google drive
googledrive::drive_update("dynverse/paper.pdf", "manuscript/paper.pdf")
googledrive::drive_update("dynverse/paper_changes.pdf", "manuscript/paper_changes.pdf")
googledrive::drive_update("dynverse/supplementary.pdf", "manuscript/supplementary.pdf")

# prepare for submission
# paper_folder <- "../../dyndocs/20180401_submission_nat_biotech/"
# file.copy("analysis/paper/paper_latex.pdf", paste0(paper_folder, "paper.pdf"), overwrite=TRUE)
# file.copy("analysis/paper/supplementary_latex.pdf", paste0(paper_folder, "supplementary_material.pdf"), overwrite=TRUE)
#
# figs <- read_rds(derived_file("figs.rds", "paper"))
# figs$fig_id <- seq_len(nrow(figs))
#
# walk2(figs$fig_id, figs$fig_path, function(fig_id, fig_path) {
#   file.copy(fig_path, paste0(paper_folder, glue::glue("fig_{fig_id}.pdf")), overwrite=TRUE)
# })
