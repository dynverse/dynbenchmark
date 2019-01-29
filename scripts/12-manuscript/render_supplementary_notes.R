#' Renders the supplementary notes in pdf

library(dynbenchmark)
library(tidyverse)

supplementary_note_paths <- c(
  result_file("README.Rmd", experiment = "02-metrics"),
  result_file("README.Rmd", experiment = "01-datasets/02-synthetic")
)

walk(supplementary_note_paths, function(supplementary_note_path) {
  print(paste0("Processing ", supplementary_note_path))
  rmarkdown::render(
    supplementary_note_path,
    output_format = dynbenchmark::pdf_supplementary_note(),
    quiet = TRUE
  )
})
