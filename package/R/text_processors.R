# append pre knit function to the current pre knit functions
append_pre_processor <- function(format, func) {
  pre_processor2 <- format$pre_processor
  function(metadata, input_file, runtime, knit_meta, files_dir , output_dir) {
    if (is.function(pre_processor2)) {pre_processor2(metadata, input_file, runtime, knit_meta, files_dir , output_dir)}
    func(metadata, input_file, runtime, knit_meta, files_dir , output_dir)
    invisible()
  }
}

# read the file, apply the pre knit, and save the file
apply_pre_processor <- function(func) {
  function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    readr::read_lines(input_file) %>% func() %>% readr::write_lines(input_file)
    invisible()
  }
}


process_changes <- function(x, render_changes = TRUE, format = get_default_format()) {
  if (format == "latex") {
    if (render_changes) {
      x %>%
        stringr::str_replace_all("\U2192", "\\\\changes{") %>%
        stringr::str_replace_all("\U2190", "}")
    } else {
      x %>%
        stringr::str_replace_all("\U2192", "") %>%
        stringr::str_replace_all("\U2190", "")
    }
  } else {
    x
  }
}

process_header_newline <- function(x) {
  x %>%
    stringr::str_replace_all("^#", "\n#")
}

#' Process relative paths to links & figures
#' First extract every link, determine whether it is a relative path and if yes, add folder to the front
#'
#' @param knit Character vector
#' @param folder The relative folder
#' @examples
#' knit <- c(
#' "hshlkjdsljkfdhg [i am a absolute path](/pompompom/dhkjhlkj/) kjfhlqkjsdhlkfjqsdf",
#' "hshlkjdsljkfdhg [i am a relative path](pompompom/dhkjhlkj/) kjfhlqkjsdhlkfjqsdf",
#' "<img src = \"heyho/heyho\">",
#' "<img src = \"/heyho/heyho\">"
#' )
#' dynbenchmark:::fix_relative_paths(knit, "IT WORKED :)")
fix_relative_paths <- function(knit, folder) {
  patterns <- c(
    "(\\[[^\\]]*\\]\\()([^\\)]*)(\\))",
    "(src[ ]?=[ ]?[\"\'])([^\"\']*)([\"\'])"
  )

  for (pattern in patterns) {
    knit <- knit %>%
      str_replace_all(
        pattern,
        function(link) {
          matches <- stringr::str_match(link, pattern)
          prefix <- matches[2]
          file <- matches[3] # contains the file
          suffix <- matches[4]

          # do not fix absolute paths, urls or anchors
          if (fs::is_absolute_path(file) || startsWith(file, "http") || startsWith(file, "#")) {
            link
          } else {
            glue::glue("{prefix}{folder}/{file}{suffix}")
          }
        }
      )
  }

  knit
}


fix_references_header <- function(knit) {
  knit %>% str_replace_all("^#*.*References.*", "#### References")
}
