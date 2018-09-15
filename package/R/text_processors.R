process_changes <- function(x, format = get_default_format()) {
  if (format == "latex") {
    x %>%
      stringr::str_replace_all("\U2192", "\\textbf{") %>%
      stringr::str_replace_all("\U2192", "}")
  }
}
