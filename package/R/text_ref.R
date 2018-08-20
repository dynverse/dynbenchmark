#' Setup the text references
#' @export
setup_refs <- function() {
  tibble(ref_id = character(), name = character(), ref_type = character())
}

#' Refer to a figure or table
#'
#' @param ref_type fig, sfig, table, ...
#' @param ref_id The identifier
#' @param suffix Adding something to the iindex
#' @param anchor Whether to ancher here
#' @param pattern What to use as pattern
#'
#' @export
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
  ref_full_name <- paste0(ref_name, glue::glue_collapse(suffix, ','))
  ref <- pritt(pattern)
  if (anchor)  ref <- pritt("{ref}{anchor(ref_type, ref_id)}")
  ref
}
anchor <- function(ref_type, ref_id, format = "latex") {
  case_when(
    format == "html" ~ pritt("<a name = '{ref_type}_{ref_id}'></a>"),
    format == "latex" ~ pritt("\\protect\\hypertarget{{{ref_type}_{ref_id}}}{{}}")
  )
}




#' @rdname setup_refs
#' @export
setup_figs <- function() {
  tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric())
}

create_names <- list(
  sfig = function(i) pritt("Supplementary Figure {i}"),
  fig = function(i) pritt("Figure {i}"),
  snote = function(i) pritt("Supplementary Note {i}"),
  table = function(i) pritt("Table {i}"),
  stable = function(i) pritt("Supplementary Table {i}"),
  section = function(i) pritt("")
)

#' Add a figure
#'
#' @inheritParams ref
#' @param fig_path Path to the figure
#' @param caption_main Caption title
#' @param caption_text Caption text
#' @param width Width in inches
#' @param height Height in inches
#' @param format The format, in html or latex
#'
#' @export
add_fig <- function(fig_path, ref_id, caption_main, caption_text, width = 5, height = 7, format = "latex") {
  # save it because why not
  figs <<- figs %>% add_row(
    ref_id = ref_id,
    fig_path = fig_path,
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height
  )
  plot_fig("fig", ref_id, fig_path, caption_main, caption_text, width, height, format = format)
}


plot_fig <- function(ref_type, ref_id, fig_path, caption_main, caption_text, width = 5, height = 7, format = "latex") {
  fig_anch <- anchor(ref_type, ref_id)

  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)

  if (format == "latex") {
    # convert svg to pdf
    if (fs::path_ext(fig_path) == "svg") {
      new_fig_path <- fig_path
      fs::path_ext(new_fig_path) <- "pdf"
      system(glue::glue("inkscape {fig_path} --export-pdf={new_fig_path}"))
      fig_path <- new_fig_path
    }

    fig_name <- ref(ref_type, ref_id, pattern = "{ref_full_name}")
    subchunk <- glue::glue(
      "\\begin{{myfigure}}{{{ifelse(ref_type == 'fig', '!htbp', 'H')}}}\n",
      "\\begin{{center}}\n",
      "{fig_anch}\n",
      "\\includegraphics{{{fig_path}}}\n\n",
      "\\end{{center}}\n",
      "\\textbf{{ {fig_name}: {caption_main}}} {caption_text}\n\n",
      "\\end{{myfigure}}\n"
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
