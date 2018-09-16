#' Show a url
#'
#' @param url The url
#' @param text The text
#' @param format The format
url <- function(url, text = url, format = get_default_format()) {
  if(format == "latex") {
    pritt("\\href{{{url}}}{{{text}}}")
  } else {
    pritt("[{text}]({url})")
  }
}
