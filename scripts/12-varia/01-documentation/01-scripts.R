library(dynbenchmark)
library(fs)
library(tidyverse)

experiment("12-varia/01-documentation")

script_locations <- dir_ls("scripts", recursive = TRUE)
scripts <- tibble(
  location = script_locations
) %>%
  filter(!str_detect(location, ".*\\.md")) %>%
  filter(!str_detect(location, ".*\\.Rmd")) %>%
  filter(!is_link(location)) %>%
  mutate(
    file = path_file(location),
    parent = path_file(path_dir(location)),
    type = case_when(is_dir(location) ~ "dir", TRUE ~ "script"),
    ix = suppressWarnings(as.integer(gsub("([0-9]{2}).*", "\\1", file))),
    level = str_count(location, "/"),
    id = gsub("[0-9]{2}-(.*)", "\\1", file)
  )

extract_title <- function(location) {
  if (is_dir(location)) {
    readme_location <- path(location, "README.md")
    if (file_exists(readme_location)) {
      first_line <- read_lines(readme_location) %>% first()
      gsub("[# ]*(.*)", "\\1", first_line)
    } else {
      ""
    }
  } else {
    first_line <- read_lines(location) %>% first()

    if (str_detect(first_line, "##.*")) {
      gsub("[# ]*(.*)", "\\1", first_line)
    } else {
      ""
    }
  }
}

scripts <- scripts %>%
  mutate(
    title = map_chr(location, extract_title)
  )

# save scripts
write_rds(scripts, derived_file("scripts.rds"))
