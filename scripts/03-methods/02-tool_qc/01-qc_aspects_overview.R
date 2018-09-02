library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("03-methods/02-tool_qc")

qc_checks <- readRDS(result_file("qc_checks.rds", "03-methods"))

library(kableExtra)

invisible_grouper <- c(latex="\\iffalse {label_long(aspect_id)} \\fi {.}", html = "<span style='display: none;'>{aspect_id}</span> {.}")

citation_format <- c(
  latex=function(references) {
    if(is.na(references)) {
      ""
    } else {
      references %>%
        gsub("@", "", .) %>%
        str_split("[, \\n]+") %>%
        first() %>%
        glue::glue_collapse(",") %>%
        paste0("\\cite{", ., "}")
    }
  },
  markdown = function(references) {if(is.na(references)) {""} else {references}}
)

qc_aspects <- qc_checks %>%
  group_by(!!!rlang::syms(colnames(qc_checks)[1:which(colnames(qc_checks) == "references")])) %>%
  summarise() %>%
  ungroup() %>%
  arrange(match(aspect_id, qc_checks$aspect_id))

qc_items <- qc_checks %>%
  group_by(aspect_id) %>%
  summarise(items = glue::glue("{item} ({item_weight}).") %>% glue::glue_collapse("   "))

qc_aspects <- qc_aspects %>%
  left_join(qc_items, "aspect_id")

table <- map(c("latex", "markdown"), function(format) {
  table <- qc_aspects %>%
    mutate(
      references = map_chr(references, citation_format[[format]])
    ) %>%
    select(name, category, weight, references, items) %>%
    knitr::kable(format)
    #
    # mutate(
    #   weight = cell_spec(
    #     weight %>% {pritt(invisible_grouper[[format]])}, # add an invisible grouping indicator
    #     format,
    #     color = spec_color(weight,option="viridis",direction=-1),
    #     escape=FALSE
    #   )
    # ) %>%
    # mutate(
    #   references = map_chr(references, citation_format[[format]])
    # ) %>%
    # mutate(
    #   category = cell_spec(
    #     label_long(category),
    #     format,
    #     color = set_names(qc_categories$color, qc_categories$category)[category],
    #     escape=FALSE
    #   )
    # ) %>%
    # select(category, aspect, weight, references, item, item_weight) %>%
    # rename_all(label_long) %>%
    # knitr::kable(format, escape=FALSE) %>%
    # kable_styling(bootstrap_options = "striped", latex_options = c("scale_down")) %>%
    # landscape() %>%
    # collapse_rows(1:4)

  # if (format == "html") {
  #   for (category in unique(checks$category)) {
  #     color <- qc_categories %>% filter(category == !!category) %>% pull(color)
  #     table <- table %>%
  #       group_rows(
  #         label_long(category),
  #         min(which(checks$category == category)),
  #         max(which(checks$category == category)),
  #         label_row_css = pritt("background-color: {color}; color: #fff;")
  #     )
  #   }
  # }


  table
}) %>% set_names(c("latex", "markdown"))
table
table %>% write_rds(result_file("qc_aspects_overview.rds"))
