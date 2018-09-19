library(googlesheets)
library(tidyverse)
library(dynbenchmark)
library(xlsx)

experiment("03-methods")

tools <- read_rds(result_file("tools.rds"))

tools <- tools %>%
  arrange(date)

tools %>%
  select(tool_id, tool_name, platform, date) %>%
  as.data.frame() %>%
  write.xlsx(result_file("tools.xlsx"), row.names = FALSE)


#
# ##  ............................................................................
# ##  Paper table                                                             ####
# superscript <- c(latex = function(x) pritt("\\textsuperscript{{{x}}}"), html = function(x) pritt("<sup>{x}</sup>"))
#
# citation_format <- c(
#   latex = function(references) {
#     if(is.na(references)) {
#       ""
#     } else {
#       references %>%
#         gsub("@", "", .) %>%
#         str_split("[, \\n]+") %>%
#         first() %>%
#         glue::glue_collapse(",") %>%
#         paste0("\\cite{", ., "}")
#     }
#   },
#   html = function(references) {
#     if(is.na(references)) {
#       ""
#     } else {
#       references %>%
#         str_split("[, \\n]+") %>%
#         first() %>%
#         paste0("@", .) %>%
#         glue::glue_collapse(";") %>%
#         {pritt("[{.}]")}
#     }
#   }
# )
#
# imp_table <- map(c("latex", "html"), function(format) {
#   tools_table <-
#     tools %>%
#     filter(type == "algorithm") %>%
#     arrange(date) %>%
#     mutate(
#       evaluated = kableExtra::cell_spec(
#         ifelse(evaluated, "Yes", "No"),
#         format,
#         background = ifelse(evaluated == "Yes", "#666666", "white"),
#         color = ifelse(evaluated == "Yes", "white", "black"),
#         escape = FALSE
#       ),
#       date = strftime(date, "%d/%m/%Y"),
#       most_complex_trajectory_type =
#         kableExtra::cell_spec(
#           label_long(most_complex_trajectory_type),
#           format,
#           background = dynwrap::trajectory_types$colour[match(most_complex_trajectory_type, dynwrap::trajectory_types$id)] %>%
#             coalesce("#666666"),
#           color = "#FFFFFF"
#         ),
#       # tool_name = kableExtra::cell_spec(
#       #   tool_name,
#       #   format,
#       #   link = code_location
#       # ),
#       reference = kableExtra::cell_spec(
#         map_chr(bibtex, citation_format[[format]]),
#         format,
#         escape = FALSE
#       )
#     ) %>%
#     select(method = tool_name, date, most_complex_trajectory_type, evaluated, reference) %>%
#     rename_all(label_long)
#
#   # force newline most complex trajectory type -_-
#   if(format == "latex") {
#     tools_table <- tools_table %>%
#       rename_at(label_long("most_complex_trajectory_type"), ~paste0("\\pbox{20cm}{", label_wrap(., 20, "\\\\[-0.5em]"), "}")) %>%
#       mutate_all(~gsub("\\#", "\\\\#", .))
#   }
#
#   table <- tools_table %>%
#     knitr::kable(format, escape = F) %>%
#     kableExtra::kable_styling(bootstrap_options = c("striped", "hover","condensed"), font_size = ifelse(format == "latex", 7, 12))
#   table
# }) %>% set_names(c("latex", "html"))
# imp_table
# write_rds(imp_table, result_file("tools_table.rds"))
