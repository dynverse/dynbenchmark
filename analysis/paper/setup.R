# knitr options
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  echo = FALSE,
  comment = "#>",
  fig.path = paste0(rprojroot::find_rstudio_root_file(), "/analysis/paper/.scratch/")
)


# load libraries
library(dynalysis)
library(tidyverse)
library(cowplot)

# refering to figures & supplementary figures
refs <- tibble(ref_id = character(), name = character(), ref_type = character())
ref <- function(ref_type, ref_id, suffix="", anchor=FALSE, pattern = "[**{ref_full_name}**](#{ref_type}_{ref_id})") {
  if(nrow(refs %>% filter(ref_id == !!ref_id)) == 0) {
    refs <<- refs %>% bind_rows(tibble(
      name = create_names[[ref_type]](sum(refs$ref_type == ref_type) + 1),
      ref_type = ref_type,
      ref_id = ref_id
    ))
  }
  ref_name <- refs %>% filter(ref_id == !!ref_id) %>%
    pull(name)
  ref_full_name <- paste0(ref_name, glue::collapse(suffix, ','))
  ref <- pritt(pattern)
  if (anchor)  ref <- pritt("{ref}{anchor(ref_type, ref_id)}")
  ref
}
anchor <- function(ref_type, ref_id) {
  if (params$table_format == "html") {
    pritt("<a name='{ref_type}_{ref_id}'></a>")
  } else {
    pritt("\\hypertarget{{{ref_type}_{ref_id}}}{{}}")
  }
}
create_names <- list(
  sfig = function(i) pritt("Supplementary Figure {i}"),
  fig = function(i) pritt("Figure {i}"),
  snote = function(i) pritt("Supplementary Note {i}"),
  table = function(i) pritt("Table {i}"),
  stable = function(i) pritt("Supplementary Table {i}"),
  section = function(i) pritt("")
)
sfigs <- tibble(ref_id = character(), fig = list(), caption = character(), width=numeric(), height=numeric())
add_sfig <- function(fig, ref_id, caption, width=15, height=10) {
  sfigs <<- sfigs %>% add_row(
    fig = list(fig),
    ref_id = ref_id,
    caption=caption,
    width=width,
    height=height
  )
}

stables <- tibble(ref_id = character(), table = list(), caption = character())
add_stable <- function(table, ref_id, caption) {
  stables <<- stables %>% add_row(
    table = list(table),
    ref_id = ref_id,
    caption=caption
  )
}

# load data
methods <- read_rds(derived_file("methods.rds", experiment_id="4-method_characterisation"))
methods_evaluated <- read_rds(derived_file("methods_evaluated.rds", experiment_id="4-method_characterisation"))
implementations <- read_rds(derived_file("implementations.rds", experiment_id="4-method_characterisation"))

# citate
cite_methods <- function(method_ids) {
  implementations %>% filter(implementation_id %in% !!method_ids) %>% pull(bibtex) %>% discard(is.na) %>% {glue::glue("@{.}")} %>% glue::collapse("; ") %>% glue::glue("[", ., "]")
}

# add caption to table
add_caption_latex <- function(table, caption="hi") {
  table %>% gsub("(egin\\{table\\}\\[H\\])", paste0("\\1\n\\\\caption{", caption, "}"), .)
}

# table
add_table <- function(ref_id, table, caption) {
  if (params$table_format == "latex") {
    caption_latex <- paste0("\\\\textbf{", ref('stable', ref_id, pattern = "{ref_full_name}"), "} ", caption)
    table_output <- paste("", "", table[["latex"]] %>% add_caption_latex(caption_latex), "", "", sep="\n")
  } else {
    table_output <- paste0(
      paste0(ref("table", "implementations", anchor=TRUE), " ", caption),
      table[["html"]],
      collapse="\n"
    )
  }
  cat(table_output)
}
