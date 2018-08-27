#' Setup the text references
#' @export
setup_refs <- function() {
  tibble(ref_id = character(), name = character(), ref_type = character())
}

#' Refer to a figure or table
#'
#' @param ref_type fig, sfig, table, ...
#' @param ref_id The identifier
#' @param suffix Adding something to the index
#' @param anchor Whether to anchor here
#' @param pattern What to use as pattern
#'
#' @export
ref <- function(ref_type, ref_id, suffix = "", anchor = FALSE, pattern = "[**{ref_full_name}**](#{ref_type}_{ref_id})", format = get_default_format()) {
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
  if (anchor)  ref <- pritt("{ref}{anchor(ref_type, ref_id, format = format)}")
  ref
}
anchor <- function(ref_type, ref_id, format = get_default_format()) {
  case_when(
    format == "html" ~ pritt("<a name = '{ref_type}_{ref_id}'></a>"),
    format == "latex" ~ pritt("\\protect\\hypertarget{{{ref_type}_{ref_id}}}{{}}"),
    format == "markdown" ~ pritt("<a name = '{ref_type}_{ref_id}'></a>")
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

get_default_format <- function() {
  case_when(
    (knitr::opts_knit$get("rmarkdown.pandoc.to") %||% FALSE) == "latex"  ~ "latex",
    TRUE ~ "markdown"
  )
}


##  ............................................................................
##  Figures                                                                 ####

#' @rdname setup_refs
#' @export
setup_figs <- function() {
  tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric())
}

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
add_fig <- function(
  fig_path,
  ref_id,
  caption_main,
  caption_text,
  width = 5,
  height = 7,
  format = get_default_format()
) {
  # save it because why not
  figs <<- figs %>% add_row(
    ref_id = ref_id,
    fig_path = fig_path,
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height
  )

  plot_fig("fig", ref_id, fig_path, caption_main, caption_text, width, height, format)
}


plot_fig <- function(
  ref_type,
  ref_id,
  fig_path,
  caption_main,
  caption_text,
  width = 5,
  height = 7,
  format = "latex"
) {
  fig_anch <- anchor(ref_type, ref_id)

  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)

  # plot figure if rds
  if (fs::path_ext(fig_path) == "rds") {
    fig_path_new <- fig_path

    if (format == "latex") {
      fs::path_ext(fig_path_new) <- "pdf"

      ggsave(fig_path_new, read_rds(fig_path), width = width, height = height, device = grDevices::cairo_pdf)
      fig_path <- fig_path_new
    } else {
      fs::path_ext(fig_path_new) <- "png"

      ggsave(fig_path_new, read_rds(fig_path), width = width, height = height)
      fig_path <- fig_path_new
    }
  }

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
  } else if (format %in% c("html", "markdown")){
    fig_cap <- ref(ref_type, ref_id, pattern = "{ref_full_name}")
    subchunk <- glue::glue(
      "<p>\n",
      "  {fig_anch}\n",
      "  <img src = \"{fig_path}\" />\n",
      "</p><p>\n",
      "  <strong>{fig_cap}: {caption_main}</strong> {caption_text}\n",
      "</p>\n"
    )
  } else {
    stop("Invalid format for figures")
  }

  cat(subchunk)
}





##  ............................................................................
##  Tables                                                                  ####

#' @rdname setup_refs
#' @export
setup_tables <- function() {
  tibble(ref_id = character(), latex = list(), html = list(), markdown = list(), caption_main = character(), caption_text = character())
}

#' Add a table
#'
#' @inheritParams ref
#' @inheritParams add_fig
#' @param table Either a tibble, a path to a table or a named list with latex, html and markdown kables
#' @param html The html table
#' @param markdown The markdown table
#' @param caption_main Caption title
#' @param caption_text Caption text
#' @param format The format, in html or latex
#'
#' @export
add_table <- function(
  table,
  ref_id,
  caption_main,
  caption_text = "",
  format = get_default_format()
) {
  # load in the table if character
  if (is.character(table) && fs::path_ext(table) == "rds") {
    table <- read_rds(table)
  }

  # convert tibble to kables
  if (is_tibble(table)) {
    table <- list(
      html = knitr::kable(table, "html"),
      latex = knitr::kable(table, "latex"),
      markdown = knitr::kable(table, "markdown")
    )
  }

  # markdown = html if markdown not given
  if (is.null(table$markdown)) {
    table$markdown <- table$html
  }

  # make sure all tables are given
  testthat::expect_setequal(names(table), c("html", "latex", "markdown"))


  # save it because why not
  tables <<- tables %>% add_row(
    latex = table$latex,
    html = table$html,
    markdown = table$markdown,
    ref_id = ref_id,
    caption_main = caption_main,
    caption_text = caption_text
  )

  # anchor
  table_anch <- anchor("table", ref_id)

  # caption
  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)
  table_name <- ref("table", ref_id, pattern = "{ref_full_name}")

  # render the table
  if (format == "latex") {
    subchunk <- glue::glue(
      "\\begin{{table}}[H]\n",
      "\\begin{{center}}\n",
      "{table_anch}\n",
      "{table$latex %>% glue::glue_collapse('\n')}\n\n",
      "\\end{{center}}\n",
      "\\textbf{{ {table_name}: {caption_main}}} {caption_text}\n\n",
      "\\end{{table}}\n"
    )
  } else if (format == "html") {
    subchunk <- glue::glue(
      "<p>\n",
      "  {table_anch}\n",
      "  {table$html %>% glue::glue_collapse('\n')}\n",
      "</p><p>\n",
      "  <strong>{table_name}: {caption_main}</strong> {caption_text}\n",
      "</p>\n"
    )
  } else if (format == "markdown") {
    subchunk <- glue::glue(
      "\n",
      "  {table_anch}",
      "\n\n",
      "  {table$markdown %>% glue::glue_collapse('\n')}",
      "\n\n",
      "  **{table_name}: {caption_main}** {caption_text}",
      "\n\n"
    )
  }

  cat(subchunk)
}
