#' A nested github markdown document
#'
#' @param ... Parameters for rmarkdown::github_document
#'
#' @export
github_markdown_nested <- function(...) {
  format <- rmarkdown::github_document(...)
  format$pandoc$args <- c(format$pandoc$args, "--atx-headers")
  format
}


#' A supplementary note document
#'
#' @param ... Parameters for rmarkdown::pdf_document
#'
#' @export
pdf_supplementary_note <- function(...) {
  format <- rmarkdown::pdf_document(...)
  format
}
