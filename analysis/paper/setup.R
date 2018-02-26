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
ref <- function(ref_type, ref_id, suffix="", anchor=FALSE) {
  if(nrow(refs %>% filter(ref_id == !!ref_id)) == 0) {
    refs <<- refs %>% bind_rows(tibble(
      name = create_names[[ref_type]](sum(refs$ref_type == ref_type) + 1),
      ref_type = ref_type,
      ref_id = ref_id
    ))
  }
  ref <- refs %>% filter(ref_id == !!ref_id) %>%
    pull(name) %>%
    {pritt("[**{.}{glue::collapse(suffix, ',')}**](#{ref_type}_{ref_id})")}

  if (anchor)  ref <- pritt("{ref}{anchor(ref_type, ref_id)}")
  ref
}
anchor <- function(ref_type, ref_id) {
  pritt("<a name='{ref_type}_{ref_id}'></a>")
}
create_names <- list(
  sfig = function(i) pritt("Supplementary Figure {i}"),
  fig = function(i) pritt("Figure {i}"),
  snote = function(i) pritt("Supplementary Note {i}"),
  table = function(i) pritt("Table {i}"),
  stable = function(i) pritt("Supplementary Table {i}")
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

# citate
cite_methods <- function(method_ids) {
  methods %>% filter(method_id %in% !!method_ids) %>% glue::glue_data("@{bibtex}") %>% glue::collapse("; ") %>% glue::glue("[", ., "]")
}
