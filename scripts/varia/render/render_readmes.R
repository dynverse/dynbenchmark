#' Renders all the READMEs in the repository

library(dynbenchmark)
library(fs)
library(tidyverse)

readme_paths <- c(
  fs::dir_ls("scripts", regexp = "README\\.Rmd", recursive = TRUE),
  fs::dir_ls("derived", regexp = "README\\.Rmd", recursive = TRUE),
  # fs::dir_ls("data", regexp = "README\\.Rmd", recursive = TRUE),
  c("./README.Rmd", "results/README.Rmd")
)

walk(readme_paths, function(readme_path) {
  print(paste0("Processing ", readme_path))
  rmarkdown::render(
    readme_path,
    output_format = dynbenchmark::github_markdown_nested(html_preview = FALSE),
    quiet = TRUE
  )
})
