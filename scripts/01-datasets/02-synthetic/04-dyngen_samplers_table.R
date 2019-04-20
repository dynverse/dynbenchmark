library(dynbenchmark)
library(tidyverse)

experiment("01-datasets/02-synthetic")

#   ____________________________________________________________________________
#   Samplers table                                                          ####
samplers <- read_tsv(raw_file("samplers"))
notes <- c("$y_{max} = r/d * p/q$")

table <- map(c("latex", "html"), function(format) {
  table <- samplers %>% mutate(text = stringr::str_glue("${parameter} = {distribution}$")) %>%
    select(text) %>%
    mutate(text = kableExtra::cell_spec(text, format, escape = FALSE)) %>%
    knitr::kable(format, escape = FALSE, col.names = NULL) %>%
    kableExtra::kable_styling(bootstrap_options = "condensed") %>%
    kableExtra::footnote(notes, general_title = "where", escape = FALSE) %>%
    gsub("&amp;", "&", .)
  table
}) %>% set_names(c("latex", "html"))

write_rds(table, result_file("samplers.rds"), compress = "xz")
