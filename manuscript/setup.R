
# load libraries
library(dynbenchmark)
library(tidyverse)
library(cowplot)

# refering to figures & supplementary figures
refs <- tibble(ref_id = character(), name = character(), ref_type = character())
ref <- function(ref_type, ref_id, suffix = "", anchor = FALSE, pattern = "[**{ref_full_name}**](#{ref_type}_{ref_id})") {
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
  case_when(
    params$table_format == "html" ~ pritt("<a name = '{ref_type}_{ref_id}'></a>"),
    params$table_format == "latex" ~ pritt("\\protect\\hypertarget{{{ref_type}_{ref_id}}}{{}}")
  )
}
create_names <- list(
  sfig = function(i) pritt("Supplementary Figure {i}"),
  fig = function(i) pritt("Figure {i}"),
  snote = function(i) pritt("Supplementary Note {i}"),
  table = function(i) pritt("Table {i}"),
  stable = function(i) pritt("Supplementary Table {i}"),
  section = function(i) pritt("")
)
sfigs <- tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric())
add_sfig <- function(fig_path, ref_id, caption_main, caption_text, width = 15, height = 10) {
  sfigs <<- sfigs %>% add_row(
    ref_id = ref_id,
    fig_path = fig_path,
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height
  )
}

figs <- tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric())
add_fig <- function(fig_path, ref_id, caption_main, caption_text = "", width = 5, height = 7) {
  # save it because why not
  figs <<- figs %>% add_row(
    ref_id = ref_id,
    fig_path = fig_path,
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height
  )
  plot_fig("fig", ref_id, fig_path, caption_main, caption_text, width, height)
}
plot_fig <- function(ref_type, ref_id, fig_path, caption_main, caption_text, width = 5, height = 7) {
  fig_anch <- anchor(ref_type, ref_id)

  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)

  if (params$table_format == "latex") {
    fig_name <- ref(ref_type, ref_id, pattern = "{ref_full_name}")
    subchunk <- glue::glue(
      "\\Begin{{myfigure}}{{{ifelse(ref_type == 'fig', '!htbp', 'H')}}}\n",
      "\\Begin{{center}}\n",
      "{fig_anch}\n",
      "\\includegraphics{{{fig_path}}}\n\n",
      "\\End{{center}}\n",
      "**{fig_name}: {caption_main}** {caption_text}\n\n",
      "\\End{{myfigure}}\n"
    )
  } else {
    fig_cap <- ref(ref_type, ref_id, pattern = "{ref_full_name}")
    subchunk <- glue::glue(
      "<p>\n",
      "  {fig_anch}\n",
      "  <img src = \"{fig_path}\" />\n",
      "</p><p>\n",
      "  <strong>{fig_cap}: {caption_main}</strong> {caption_text}\n",
      "</p>\n"
    )
  }

  cat(subchunk)
}

stables <- tibble(ref_id = character(), table = list(), caption = character())
add_stable <- function(table, ref_id, caption) {
  stables <<- stables %>% add_row(
    table = list(table),
    ref_id = ref_id,
    caption = caption
  )
}

# load data
methods <- read_rds(result_file("methods.rds", experiment_id = "03-methods"))
methods_evaluated <- read_rds(result_file("methods_evaluated.rds", experiment_id = "03-methods"))
tools <- read_rds(result_file("tools.rds", experiment_id = "03-methods"))
datasets_info <- read_rds(result_file("datasets_info.rds", experiment_id = "06-optimise_parameters/3-evaluate_parameters"))

# citate
cite_methods <- function(method_ids) {
  tools %>% filter(tool_id %in% !!method_ids) %>% pull(bibtex) %>% discard(is.na) %>% {glue::glue("@{.}")} %>% glue::collapse("; ") %>% glue::glue("[", ., "]")
}

# add caption to table
add_caption_latex <- function(table, caption = "hi") {
  table %>% gsub("(egin\\{table\\}\\[H\\])", paste0("\\1\n\\\\caption{", caption, "}"), .)
}

# table
add_table <- function(ref_id, table, caption) {
  if (params$table_format == "latex") {
    caption_latex <- paste0("\\\\textbf{", ref('table', ref_id, pattern = "{ref_full_name}"), "}: ", caption)
    table_output <- paste("", "", table[["latex"]] %>% add_caption_latex(caption_latex), "", "", sep = "\n")
  } else {
    table_output <- paste0(
      paste0(ref("table", "tools", anchor = TRUE), ": ", caption),
      table[["html"]],
      collapse = "\n"
    )
  }
  cat(table_output)
}

# url
url <- function(url, text) {
  if(params$table_format == "latex") {
    pritt("\\\\href{{{url}}}{{{text}}}")
  } else {
    pritt("[{text}]({url})")
  }
}
