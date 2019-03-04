#' Renders results READMEs

library(dynbenchmark)
library(fs)
library(tidyverse)

readme_paths <- c(
  fs::dir_ls("results", regexp = "README\\.Rmd", recursive = TRUE)
  # fs::dir_ls(result_file("", experiment = "03-methods"), regexp = "README\\.Rmd", recursive = TRUE)
  # "results/README.Rmd"
)

walk(readme_paths, function(readme_path) {
  print(paste0("Processing ", fs::path_rel(readme_path)))
  rmarkdown::render(
    readme_path,
    output_format = dynbenchmark::github_markdown_nested(html_preview = FALSE),
    quiet = TRUE
  )
})
