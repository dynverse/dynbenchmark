library(tidyverse)
library(googlesheets)

# gs_auth()

sheet <- gs_key("13jHFuMwie7oIxGcyLO95-onFri4M_GJjqgMEUnZliQI")
hypotheses <- gs_read(sheet, ws = "Hypotheses") %>% mutate(
  parent_order = gsub("\\.[0-9]*$", "", order), 
  parent_id = experiment_id[match(parent_order, order)],
  level = sapply(order, function(x) stringr::str_length(gsub("[^\\.]", "", x))),
  colstr = sapply(color, function(x) col2rgb(x)[,1] %>% paste(collapse = ","))
)

experiment_links_df <- hypotheses %>% select(parent_id, experiment_id) %>% filter(parent_id != experiment_id)

textbox_tikz <- function(x) {
  ifelse(is.na(x), "", x %>% 
      seqinr::stresc() %>% 
      gsub("\n", "\\\\\\\\\n    ", .) %>% 
      gsub("@", "\\@", .))
}
hypotheses <- hypotheses %>% mutate(
  tikznode = paste0(
    "  \\myhypothesis{", experiment_id, "}{", 
    gsub("_", "\\\\_", experiment_id), "}{", 
    colstr, "}{", 
    textbox_tikz(statement), "}{", 
    textbox_tikz(experiment_method), "}{", 
    textbox_tikz(expected_results), "}{",
    textbox_tikz(motivation), "}"
  )
)

write_lines(
  paste(hypotheses$tikznode, collapse = "\n\n"), 
  "analysis/analyses/generate_hypotheses_mindmap/hypotheses_tikz.tex"
)

