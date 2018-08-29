#' Renders all the READMEs in the repository

library(dynbenchmark)
library(fs)
library(tidyverse)

experiment("12-varia/01-documentation")

readme_paths <- fs::dir_ls("scripts", regexp = "README\\.Rmd", recursive = TRUE) %>% c("./README.Rmd")

options(knitr.duplicate.label = 'allow')

knitr::opts_chunk$set(echo = FALSE)
walk(readme_paths, function(readme_path) {
  print(paste0("Processing ", readme_path))
  rmarkdown::render(
    readme_path,
    output_format = rmarkdown::github_document(
      html_preview = FALSE,
      pandoc_args = c(
        "--atx-headers" # atx headers are necessary so that it is easy to adapt the header level
      )
    ),
    quiet = TRUE
  )
})
