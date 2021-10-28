#' Renders the manuscript, and prepares the files for submission

library(dynbenchmark)
library(tidyverse)

experiment("12-manuscript")

output_directory <- result_file()
if (fs::dir_exists(output_directory)) fs::dir_delete(output_directory)
fs::dir_create(output_directory)

# # download from google docs
# httr::set_config(httr::config(http_version = 0)) # avoid http2 framing layer bug
# drive <- googledrive::drive_download(googledrive::as_id("1Xmuhp1_EGr4Qt6kKRiJGbeQMBBr50ulteFF4YJIeKhA"), type="text/plain", overwrite=TRUE, path = tempfile())
# system(stringr::str_glue("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character added by google
# system(stringr::str_glue("sed -i 's/ *\\(\\[@[^\\]]*\\]\\)/\\1/' {drive$local_path}")) # remote spaces before citations
# system(stringr::str_glue("cat {drive$local_path} > manuscript/paper.Rmd"))

# render main manuscript
rmarkdown::render("manuscript/paper.Rmd", output_format = dynbenchmark::pdf_manuscript(), output_dir = output_directory)

rmarkdown::render("manuscript/paper.Rmd", output_format = word_manuscript())



# copy and render figs, sfigs and stables
pwalk(bind_rows(figs, sfigs), function(ref_id, ...) {
  cat(ref_id, "\n", sep = "")
  pdf_path <- plot_fig( ref_id = ref_id, format = "pdf", ...)
  name <- refs %>% filter(ref_id == !!ref_id) %>% pull(name)
  fs::file_copy(pdf_path, fs::path(output_directory, paste0(name, ".pdf")), overwrite = TRUE)
})

pwalk(stables, function(ref_id, table, ...) {
  excel_path <- table$excel
  if (!is.null(excel_path)) {
    name <- refs %>% filter(ref_type == "stable", ref_id == !!ref_id) %>% pull(name)
    fs::file_copy(excel_path, fs::path(output_directory, paste0(name, ".xlsx")), overwrite = TRUE)
  }
})

# render supplementary
rmarkdown::render("manuscript/supplementary.Rmd", output_format = pdf_supplementary_note(), output_dir = output_directory)

# download cover letter
googledrive::drive_download(
  googledrive::as_id("1p5EHvNNpSXRorwDDEIRiHn29F2yQrhnkidE1e30Www8"),
  fs::path(output_directory, "cover_letter.pdf"),
  overwrite = TRUE
)

# download rebuttal
googledrive::drive_download(
  googledrive::as_id("1KWgqwrv998yKLcyMv7Zm9o5LrwNRwbWu3gl8LTDVHpA"),
  fs::path(output_directory, "rebuttal.pdf"),
  overwrite = TRUE
)

# copy reports
fs::dir_ls(raw_file()) %>% fs::file_copy(result_file(), overwrite = TRUE)

# browse
# fs::file_show(str_glue("{output_directory}/paper.pdf"))
# fs::file_show(str_glue("{output_directory}/paper_changes.pdf"))
# fs::file_show(str_glue("{output_directory}/supplementary.pdf"))

# upload to google drive
walk(
  fs::dir_ls(output_directory),
  function(file) {
    googledrive::drive_trash(paste0("thesis/dynverse/output/", fs::path_file(file)))
    googledrive::drive_upload(file, path = "thesis/dynverse/output", name = fs::path_file(file))
  }
)
