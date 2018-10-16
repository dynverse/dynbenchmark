#' Will recursively extract documentations from the scripts and folders within a given folder
#'
#' @param folder The folder from which to start, defaults to current working directory
#' @param recursive Whether to extract scripts recursively
#' @export
extract_scripts_documentation <- function(folder = getwd(), recursive = TRUE) {
  script_locations <- fs::dir_ls(folder, recursive = recursive)
  scripts <- tibble(
    location = script_locations
  ) %>%
    filter(
      !str_detect(location, ".*\\.md") &
        !str_detect(location, ".*\\.Rmd") &
        !str_detect(location, ".*\\.Rproj") &
        !fs::is_link(location)
    ) %>%
    mutate(
      file = fs::path_file(location),
      parent = fs::path_file(fs::path_dir(location)),
      type = case_when(fs::is_dir(location) ~ "directory", TRUE ~ "script"),
      ix = suppressWarnings(as.integer(gsub("([0-9]{1,2}).*", "\\1", file))),
      subix = stringr::str_match(file, "[0-9]{1,2}([a-z]?).*")[, 2],
      level = stringr::str_count(location, "/"),
      id = gsub("[0-9]{1,2}[a-z]?-(.*)", "\\1", file)
    )

  extract_title <- function(location) {
    if (fs::is_dir(location)) {
      # extract title: from README.Rmd
      readme_location <- fs::path(location, "README.Rmd")
      if (fs::file_exists(readme_location)) {
        title_line <- readr::read_lines(readme_location) %>%
          str_subset("^# .*") %>%
          first()

        if (length(title_line)) {
          stringr::str_replace(title_line, "^# (.*)$", "\\1")
        } else {
          ""
        }
      } else {
        ""
      }
    } else {
      # extract title from scripts #'
      first_line <- readr::read_lines(location) %>% first()

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
  if(is.null(knitr::opts_knit$get("output.dir"))) {knitr::opts_knit$set("output.dir" = ".")} # fix for bizaroo knit_child error

  # print table of scripts
  extract_scripts_documentation(folder, recursive = recursive) %>%
    arrange(ix) %>%
    mutate(
      symbol = case_when(type == "directory" ~ "\U1F4C1", type == "script" ~ "\U1F4C4", TRUE ~ ""),
      location = glue::glue("[{symbol}`{id}`]({file})"),
      order = paste0(ifelse(is.na(ix), "", ix), ifelse(is.na(subix), "", subix)),
      description = map_chr(title, ~if(. != ""){knitr::knit_child(text = ., quiet = TRUE)}else{""})
    ) %>%
    select(`\\#` = order, `script/folder` = location, description) %>%
    knitr::kable()

}


#' Generates a link to the results
#'
#' @param scripts_folder Folder from which to calculate the experiment id
#' @export
link_to_results <- function(scripts_folder = getwd()) {
  experiment_id <- scripts_folder %>% gsub(".*/?scripts/(.*)", "\\1", .)
  results_dir <- scripts_folder %>% gsub("scripts", "results", .)
  results_location <- paste0("https://github.com/dynverse/dynbenchmark_results/tree/master/", experiment_id)

  if (fs::is_absolute_path(experiment_id) || !file.exists(results_dir)) {
    warning(results_dir, " does not exist! Cannot link to results")
    NA
  } else {
    results_location
  }
}
