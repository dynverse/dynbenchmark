library(dynbenchmark)
library(tidyverse)

experiment("gource")

gitlinks_folder <- derived_file("git_links/")
output_folder <- derived_file("git_output/")

dir.create(output_folder)

git_df <-
  readr::read_lines("../.gitmodules") %>%
  keep(~ grepl("submodule", .)) %>%
  gsub(".*\"(.*)\".*", "\\1", .) %>%
  data_frame(path = ., prefix = paste0("/", .), filename = str_replace(., "/", "_"), gitmodule = TRUE) %>%
  add_row(path = "..", prefix = "", filename = "dynverse", gitmodule = FALSE) %>%
  add_row(path = "../../dyndocs", prefix = "/dyndocs", filename = "dyndocs", gitmodule = FALSE) %>%
  add_row(path = "../../projects/qsub/", prefix = "/libraries/qsub", filename = "libraries_qsub", gitmodule = FALSE)

unlink(gitlinks_folder, recursive = T)
for (i in seq_len(nrow(git_df))) {
  path <- git_df$path[[i]]
  prefix <- git_df$prefix[[i]]
  filename <- git_df$filename[[i]]
  gitmodule <- git_df$gitmodule[[i]]

  gource_output <- paste0(
    output_folder, "/",
    filename,
    ".txt"
  )

  if (gitmodule) {
    dir.create(paste0(gitlinks_folder, path), recursive = T, showWarnings = F)
    file.symlink(
      normalizePath(paste0("../.git/modules/", path)),
      paste0(gitlinks_folder, path, "/.git")
    )
    git_location <- paste0(gitlinks_folder, path)
  } else {
    git_location <- path
  }
  cmd <- paste0("gource --output-custom-log ", gource_output, " ", git_location)
  system(cmd)

  read_lines(gource_output) %>%
    str_replace("\\|/", paste0("|", prefix, "/")) %>%
    str_replace("\\.REMOVED\\.git-id$", "") %>%
    write_lines(gource_output)
}
unlink(gitlinks_folder, recursive = T)
