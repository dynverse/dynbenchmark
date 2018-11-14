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
#' @param format The output format (html, latex, markdown, ...)
#'
#' @export
ref <- function(ref_type, ref_id, suffix = "", anchor = FALSE, format = get_default_format()) {
  if(nrow(refs %>% filter(ref_id == !!ref_id)) == 0) {
    refs <<- refs %>% bind_rows(tibble(
      name = create_names[[ref_type]](sum(refs$ref_type == ref_type) + 1),
      ref_type = ref_type,
      ref_id = ref_id
    ))
  }
  ref_name <- refs %>% filter(ref_id == !!ref_id) %>%
    pull(name)
  ref_full_name <- paste0(ref_name, label_vector(suffix))

  # print text of ref
  if (format != "latex") {
    pattern <- "[**{ref_full_name}**](#{ref_type}_{ref_id})"
  } else {
    pattern <- "\\protect\\hyperlink{{{ref_type}_{ref_id}}}{{\\textbf{{{ref_full_name}}}}"
  }
  ref <- pritt(pattern)

  # add anchor is requested
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
  tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric(), ref_type = character())
}

#' @rdname setup_refs
#' @export
setup_sfigs <- function() {
  tibble(ref_id = character(), fig_path = character(), caption_main = character(), caption_text = character(), width = numeric(), height = numeric(), integrate = logical(), ref_type = character())
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
  caption_text = "",
  width = 5,
  height = 7,
  format = get_default_format()
) {
  # save it because why not
  figs <<- figs %>% add_row(
    ref_id = ref_id,
    ref_type = "fig",
    fig_path = if(is.character(fig_path)) {fig_path} else {list(fig_path)},
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height
  )

  plot_fig("fig", ref_id, fig_path, caption_main, caption_text, width, height, format)
}

#' @rdname add_fig
#' @param integrate Whether to integrate the figure, or direct the reader to a separate file
#' @export
add_sfig <- function(
  fig_path,
  ref_id,
  caption_main,
  caption_text = "",
  width = 5,
  height = 7,
  format = get_default_format(),
  integrate = TRUE
) {
  # save it because it's necessary
  sfigs <<- sfigs %>% add_row(
    ref_id = ref_id,
    ref_type = "sfig",
    fig_path = if(is.character(fig_path)) {fig_path} else {list(fig_path)},
    caption_main = caption_main,
    caption_text = caption_text,
    width = width,
    height = height,
    integrate = integrate
  )
}

#' @rdname add_fig
#' @export
plot_fig <- function(
  ref_type,
  ref_id,
  fig_path,
  caption_main,
  caption_text,
  width = 5,
  height = 7,
  format = "latex",
  integrate = TRUE
) {
  fig_anch <- anchor(ref_type, ref_id)

  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)

  # if fig path is an rds, or a ggplot object is given -> load into fig
  if (ggplot2::is.ggplot(fig_path)) {
    fig <- fig_path
    fig_path <- paste0(knitr::opts_chunk$get("fig.path") %||% ".", "/", ref_id, ".rds")
    dir.create(fs::path_dir(fig_path), recursive = TRUE, showWarnings = FALSE)
  } else if (fs::path_ext(fig_path) == "rds") {
    fig <- read_rds(fig_path)
  } else {
    fig <- NULL
  }

  # if fig path is a pdf and  format is html/markdown -> convert to png
  if (format %in% c("html", "markdown") && fs::path_ext(fig_path) == "pdf") {
    new_fig_path <- fs::path_ext_set(fig_path, "png")
    system(glue::glue("convert -density 300 {fig_path} {new_fig_path}"))
    fig_path <- new_fig_path
  }

  # save the figure in the appropriate format
  # plot figure if rds
  if (!is.null(fig)) {
    if (format %in% c("latex", "pdf")) {
      fig_path <- fs::path_ext_set(fig_path, "pdf")

      ggsave(fig_path, fig, width = width, height = height, device = grDevices::cairo_pdf)
    } else {
      fig_path <- fs::path_ext_set(fig_path, "png")

      ggsave(fig_path, fig, width = width, height = height)
    }
  }

  # convert to relative path (for github markdown)
  fig_path <- fs::path_rel(fig_path)

  # convert svg to pdf if required
  if (fs::path_ext(fig_path) == "svg" && format %in% c("pdf", "latex")) {
    new_fig_path <- fig_path
    fs::path_ext(new_fig_path) <- "pdf"
    system(glue::glue("inkscape {fig_path} --export-pdf={new_fig_path}"))
    fig_path <- new_fig_path
  }

  # check if path exists
  if (!file.exists(fig_path)) {
    stop(fig_path, " does not exist!")
  }

  if (format %in% "latex") {
    fig_name <- ref(ref_type, ref_id)

    include_graphics <- if (integrate) {
      "\\includegraphics[height={height/2}in, width={width/2}in]{{{fig_path}}}\n\n"
    } else {
      "This figure is provided as a separate pdf file"
    }

    subchunk <- glue::glue(
      "\\begin{{myfigure}}{{{ifelse(ref_type == 'fig', '!htbp', 'H')}}}\n",
      "\\begin{{center}}\n",
      "{fig_anch}\n",
      include_graphics,
      "\\end{{center}}\n",
      "\\textbf{{{fig_name}: {caption_main}}} {caption_text}\n\n",
      "\\end{{myfigure}}\n"
    ) %>% knitr::asis_output()

  } else if (format %in% c("html", "markdown")){
    width <- width * 70
    height = height * 70

    fig_cap <- ref(ref_type, ref_id)
    subchunk <- glue::glue(
      "\n\n\n<p>\n",
      "  {fig_anch}\n",
      "  <img src = \"{fig_path}\" width = \"{width}\" height = \"{height}\" />\n",
      "</p><p>\n",
      "  <strong>{fig_cap}: {caption_main}</strong> {caption_text}\n",
      "</p>\n",
      "___",
      "\n\n"
    ) %>% knitr::asis_output()
  } else if (format %in% "pdf") {
    subchunk <- fig_path
  } else {
    stop("Invalid format for figures")
  }

  subchunk
}





##  ............................................................................
##  Tables                                                                  ####

#' @rdname setup_refs
#' @export
setup_tables <- function() {
  tibble(ref_id = character(), table = list(), caption_main = character(), caption_text = character(), ref_type = character())
}

#' @rdname setup_refs
#' @export
setup_stables <- function() {
  tibble(ref_id = character(), table = list(), caption_main = character(), caption_text = character(), ref_type = character())
}

#' Add a table
#'
#' @param table Either a tibble, a path to a table or a named list with latex, html and markdown kables
#' @param caption_main Caption title
#' @param caption_text Caption text
#' @param format The format, in html or latex
#' @inheritParams ref
#' @inheritParams add_fig
#'
#' @export
add_table <- function(
  table,
  ref_id,
  caption_main,
  caption_text = "",
  format = get_default_format()
) {
  table <- process_table(table)

  # save it because why not
  tables <<- tables %>% add_row(
    table = list(table),
    ref_type = "table",
    ref_id = ref_id,
    caption_main = caption_main,
    caption_text = caption_text
  )

  show_table(table, ref_id, caption_main, caption_text, ref_type = "table")
}

#' @rdname add_table
#' @export
add_stable <- function(
  table,
  ref_id,
  caption_main,
  caption_text = "",
  format = get_default_format()
) {
  table <- process_table(table)

  # save it because why not
  stables <<- stables %>% add_row(
    table = list(table),
    ref_type = "stable",
    ref_id = ref_id,
    caption_main = caption_main,
    caption_text = caption_text
  )
}

process_table <- function(table) {
  # load in the table if character
  if (is.character(table) && fs::path_ext(table) == "rds") {
    table <- read_rds(table)
  }

  # if an excel, add a note that this table was submitted separately
  if (is.character(table) && fs::path_ext(table) == "xlsx") {
    if (!fs::file_exists(table)) stop(table, " does not exist!")

    table <- list(
      html = "This table is provided as a separate excel file",
      latex = "This table is provided as a separate excel file",
      markdown = "This table is provided as a separate excel file",
      excel = table
    )
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
  testthat::expect_true(all(c("latex", "markdown") %in% names(table)))

  table
}

show_table <- function(table, ref_id, caption_main, caption_text, format = get_default_format(), ref_type = "table") {
  # anchor
  table_anch <- anchor(ref_type, ref_id)

  # caption
  caption_main <- knitr::knit(text = caption_main, quiet = TRUE)
  caption_text <- knitr::knit(text = caption_text, quiet = TRUE)
  table_name <- ref(ref_type, ref_id)

  # render the table
  if (format == "latex") {
    subchunk <- glue::glue(
      "\\begin{{table}}[H]\n",
      "\\begin{{center}}\n",
      "{table_anch}\n",
      "{table$latex %>% glue::glue_collapse('\n')}\n\n",
      "\\end{{center}}\n",
      "\\textbf{{{table_name}: {caption_main}}} {caption_text}\n\n",
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

  knitr::asis_output(subchunk)
}




##  ............................................................................
##  Global refs setup                                                       ####
#' Setup the refs for a markdown document, globally!
#'
#' @export
setup_refs_globally <- function()  {
  refs <<- dynbenchmark::setup_refs()
  figs <<- dynbenchmark::setup_figs()
  sfigs <<- dynbenchmark::setup_sfigs()
  tables <<- dynbenchmark::setup_tables()
  stables <<- dynbenchmark::setup_stables()
  invisible()
}

