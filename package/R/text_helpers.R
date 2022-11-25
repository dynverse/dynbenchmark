#' Show a url
#'
#' @param url The url
#' @param text The text
#' @param format The format
#'
#' @export
print_url <- function(url, text = url, format = get_default_format()) {
  # cut off www and http
  if (text == url) {
    text <- text %>%
      gsub("https:\\/\\/", "", .) %>%
      gsub("http:\\/\\/", "", .) %>%
      gsub("www\\.", "", .) %>%
      gsub("\\/$", "", .)
  }

  if(format == "latex") {
    paste0("\\textcolor{cyan}{\\href{", url, "}{", text, "}}")
  } else {
    stringr::str_glue("[{text}]({url})")
  }
}
