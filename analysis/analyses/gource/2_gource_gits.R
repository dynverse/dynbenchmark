library(tidyverse)

gitlinks_folder <- "analysis/data/derived_data/gource/git_links/"
output_folder <- "analysis/data/derived_data/gource/git_output"

gitmodules <- readr::read_lines("../.gitmodules") %>%
  keep(~ grepl("submodule", .)) %>%
  gsub(".*\"(.*)\".*", "\\1", .)

unlink(gitlinks_folder, recursive = T)
for (gitmodule in gitmodules) {
  gource_output <- paste0(
    output_folder, "/",
    gsub("/", "_", gitmodule),
    ".txt"
  )

  dir.create(paste0(gitlinks_folder, gitmodule), recursive = T, showWarnings = F)
  file.symlink(
    normalizePath(paste0("../.git/modules/", gitmodule)),
    paste0(gitlinks_folder, gitmodule, "/.git")
  )
  cmd <- paste0("gource --output-custom-log ", gource_output, " ", gitlinks_folder, gitmodule)
  system(cmd)

  read_lines(gource_output) %>%
    gsub("(\\|[AMD]\\|)", paste0("\\1/", gitmodule), .) %>%
    write_lines(gource_output)
}
unlink(gitlinks_folder, recursive = T)

cmd <- paste0("gource --output-custom-log ", output_folder, "/dynverse.txt ..")
system(cmd)

