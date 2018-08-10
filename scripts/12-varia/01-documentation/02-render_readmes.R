library(dynbenchmark)
library(fs)
library(tidyverse)

experiment("12-varia/01-documentation")

readme_paths <- fs::dir_ls(regexp = "README\\.Rmd", recursive = TRUE)

options(knitr.duplicate.label = 'allow')

walk(readme_paths, function(readme_path) {
  rmarkdown::render(readme_path, output_format = rmarkdown::github_document(html_preview = FALSE))
})
