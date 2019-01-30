#' Renders the supplementary notes in pdf

library(dynbenchmark)
library(tidyverse)

supp_notes <- tribble(
  ~rmd_path, ~final_path,
  result_file("README.Rmd", "02-metrics"), result_file("Supplementary Note 1.pdf", "12-benchmark"),
  result_file("README.Rmd", "01-datasets/02-synthetic"), result_file("Supplementary Note 2.pdf", "12-benchmark")
) %>%
  mutate(pdf_path = str_replace_all(rmd_path, "Rmd$", "pdf"))


walk(supp_notes$rmd_path, function(supplementary_note_path) {
  print(paste0("Processing ", supplementary_note_path))
  rmarkdown::render(
    supplementary_note_path,
    output_format = dynbenchmark::pdf_supplementary_note(),
    quiet = TRUE
  )
})

pwalk(supp_notes, function(rmd_path, final_path, pdf_path) {
  file.copy(pdf_path, final_path, overwrite = TRUE)
})
