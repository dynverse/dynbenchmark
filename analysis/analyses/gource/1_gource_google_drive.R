library(dynalysis)
library(tidyverse)
library(purrr)
library(XML)

experiment("gource")

links <- read_rds(derived_file("google_drive_revisions.rds")) %>%
  mutate(name = paste0("/google_drive/", id))

# todo: also parse years
process <- function(xml, docname) {
  data <- xmlParse(file)

  id_or_class_xp <- "//div[@class='docs-revisions-tile']//text()"
  values <- xpathSApply( data, id_or_class_xp, xmlValue)

  names <- c("Helena Todorov", "Yvan Saeys", "Robrecht Cannoodt", "Wouter Saelens")
  names_ix <- which(values %in% names)
  dates_ix <- names_ix - 1
  while (any(values[dates_ix] %in% names)) {
    mehix <- values[dates_ix] %in% names
    dates_ix[mehix] <- dates_ix[mehix] - 1
  }

  df <- data_frame(
    person = values[names_ix],
    date = values[dates_ix]
  ) %>% mutate(
    #posix = as.POSIXct(date, format = "%d %B, %H:%M"),
    posix = as.POSIXct(date, format = "%B %d, %I:%M %p"),
    time = as.integer(posix),
    docname
  ) %>% arrange(time) %>% mutate(
    action = ifelse(seq_len(n()) == 1, "A", "M"),
    final = paste0(time, "|", person, "|", action, "|", docname)
  )
}

links %>%
  rowwise() %>%
  mutate(
    output_df = l
  )

df <- seq_len(nrow(links)) %>% map_df(function(i) {
  process(links$source[[i]], links$name[[i]])
}) %>% na.omit

write_lines(df$final, derived_file("google_drive.txt"))
