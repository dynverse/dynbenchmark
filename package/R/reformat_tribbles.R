#' Align tribbles in a file
#'
#' The start and end of a tribble need to be marked with a commend `# tribble_start` and `# tribble_end`.
#'
#' @param script_file The file in which to align the tribble
#' @param align The alignment (left, center, right)
#'
#' @importFrom readr read_lines write_lines
#' @export
reformat_tribbles <- function(script_file, align = c("left", "center", "right")) {
  align <- match.arg(align)

  pad_fun <- function(word, width) {
    padding <- width - nchar(word)
    nleft <- switch(
      align,
      left = 0,
      center = floor(padding / 2),
      right = padding
    )
    nright = switch(
      align,
      left = padding,
      center = ceiling(padding / 2),
      right = 0
    )
    paste(c(rep(" ", nleft), word, rep(" ", nright)), collapse = "")
  }

  lines <- readr::read_lines(script_file)
  tribble_start <- grep("tribble_start", lines)
  tribble_end <- grep("tribble_end", lines)
  for (i in seq_along(tribble_start)) {
    starti <- tribble_start[[i]] + 1
    endi <- tribble_end[[i]] - 1
    subset <-
      lines[starti:endi] %>%
      gsub(" *([\"`][^\"`]*[\"`]|[a-zA-Z_]*\\([^\\)]*\\)|[^ ,]*),? *", "\\1\U00B0", .) %>%
      map(~ strsplit(., "\U00B0")[[1]])

    numcols <- length(subset[[1]])

    maxwidths <- map_int(seq_len(numcols), function(j) {
      subset %>% map_chr(~ .[[j]]) %>% map_int(nchar) %>% max
    }) + 3

    new_subset <-
      map_chr(subset, function(line) {
        map2_chr(paste0(line, ","), maxwidths, pad_fun) %>%
          paste(collapse = "") %>%
          paste0("  ", .) %>%
          gsub(" *$", "", .)
      })

    new_subset[length(new_subset)] <-
      new_subset[length(new_subset)] %>%
      gsub(", *$", "", .)

    lines[starti:endi] <- new_subset
  }
  readr::write_lines(lines, script_file)
}
