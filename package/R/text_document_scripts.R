#' Will recursively extract documentations from the scripts and folders within a given folder
#'
#' @param folder The folder from which to start, defaults to current working directory
#' @export
extract_scripts_documentation <- function(folder = getwd(), recursive = TRUE) {
  script_locations <- fs::dir_ls(folder, recursive = recursive)
  scripts <- tibble(
    location = script_locations
  ) %>%
    filter(
      !str_detect(location, ".*\\.md") &
        !str_detect(location, ".*\\.Rmd") &
        !fs::is_link(location)
    ) %>%
    mutate(
      file = fs::path_file(location),
      parent = fs::path_file(fs::path_dir(location)),
      type = case_when(fs::is_dir(location) ~ "dir", TRUE ~ "script"),
      ix = suppressWarnings(as.integer(gsub("([0-9]{2}).*", "\\1", file))),
      level = str_count(location, "/"),
      id = gsub("[0-9]{2}-(.*)", "\\1", file)
    )

  extract_title <- function(location) {
    if (fs::is_dir(location)) {
      # extract title: from README.Rmd
      readme_location <- fs::path(location, "README.Rmd")
      if (fs::file_exists(readme_location)) {
        title_line <- read_lines(readme_location) %>%
          str_subset("^# .*") %>%
          first()

        if (length(title_line)) {
          str_replace(title_line, "^# (.*)$", "\\1")
        } else {
          ""
        }
      } else {
        ""
      }
    } else {
      # extract title from scripts #'
      first_line <- read_lines(location) %>% first()

      if (str_detect(first_line, "#'.*")) {
        gsub("#' (.*)", "\\1", first_line)
      } else {
        ""
      }
    }
  }

  scripts <- scripts %>%
    mutate(
      title = map_chr(location, extract_title)
    )

  scripts
}


#' @rdname extract_scripts_documentation
#' @export
render_scripts_documentation <- function(folder = ".", recursive = FALSE) {
  extract_scripts_documentation(folder, recursive = recursive) %>%
    arrange(ix) %>%
    mutate(
      script = glue::glue("[`{id}`]({file})"),
      order = ifelse(is.na(ix), "", ix),
      description = map_chr(title, ~knitr::knit_child(text = ., quiet = TRUE))
    ) %>%
    select(`\\#` = order, script, description) %>%
    knitr::kable()
}

#' Knit a child README, and add an extra level of headings + fix relative paths
#'
#' @param folder Subfolder where the README.Rmd is located
#' @export
knit_child_readme <- function(folder) {
  if (is.null(knitr::opts_knit$get("output.dir"))) {knitr::opts_knit$set("output.dir" = ".")}

  file <- file.path(folder, "README.md")
  if (!file.exists(file)) {
    stop(file, " does not exist!")
  }
  knit <- read_lines(file)

  # process relative paths
  knit <- knit %>%
    str_replace_all("(\\[[^\\]]*\\]\\()([^\\)]*\\))", paste0("\\1", folder, "/\\2"))

  # add extra header sublevels & add link
  knit <- knit %>%
    str_replace_all("^(# )(.*)$", paste0("\\1[\\2](", folder, ")")) %>%
    str_replace_all("^#", "##")

  # cat output
  cat(knit %>% glue::glue_collapse("\n"))

  invisible()
}
