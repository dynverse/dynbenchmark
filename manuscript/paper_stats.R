library(wordcountaddin)
library(xml2)
library(tidyverse)

paper <- read_html("analysis/paper/paper.html")
body <- paper %>% xml_child("body")


sections <- body %>%
  xml_find_first(".//section") %>%
  xml_find_all("./div")

pieces <- tibble(
  text = sections %>% xml_text,
  section = sections %>% xml_attr("id")
)
counts <- pieces$text %>%
  map(wordcountaddin:::text_stats_fn_) %>%
  bind_rows() %>%
  mutate_all(~as.numeric(gsub("([0-9\\.]*).*", "\\1", .)))
readibilities <- pieces$text %>% map(wordcountaddin:::readability_fn_)

stats <-
  bind_cols(counts, pieces) %>%
  gather("statistic", "value", -text, -section)


stats %>%
  mutate(section = factor(section, levels = pieces$section)) %>%
  ggplot(aes(section, value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~statistic, scales = "free") +
  coord_flip()
