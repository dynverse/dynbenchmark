#' A nested github markdown document
#'
#' @inheritParams common_dynbenchmark_format
#' @param ... Parameters for rmarkdown::github_document
#'
#' @export
github_markdown_nested <- function(
  bibliography = fs::path_abs(paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib")),
  csl = fs::path_abs(paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl")),
  ...
) {
  format <- rmarkdown::github_document(...)

  # make sure atx headers are used, for knit_nest
  format$pandoc$args <- c(format$pandoc$args, "--atx-headers")

  # processor before pandoc:
  # - render all equations
  format$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    readr::read_lines(input_file) %>%
      render_equations(format = "markdown") %>%
      fix_references_header() %>%
      readr::write_lines(input_file)

    invisible()
  }

  # apply common dynbenchmark format
  format <- common_dynbenchmark_format(format)

  format
}


#' A supplementary note pdf document
#'
#' @inheritParams common_dynbenchmark_format
#' @param ... Parameters for rmarkdown::pdf_document
#'
#' @export
pdf_supplementary_note <- function(
  bibliography = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib"),
  csl = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl"),
  ...
) {
  # setup the pdf format
  format <- rmarkdown::pdf_document(
    ...,
    toc = TRUE,
    includes = rmarkdown::includes(system.file("common.sty", package = "dynbenchmark")),
    latex_engine = "xelatex",
    number_sections = FALSE
  )

  format <- common_dynbenchmark_format(
    format,
    bibliography = bibliography,
    csl = csl
  )

  format
}



#' The manuscript pdf document
#'
#' @inheritParams common_dynbenchmark_format
#' @param render_changes Whether to export a *_changes.pdf as well
#' @param ... Parameters for rmarkdown::pdf_document
#'
#' @export
pdf_manuscript <- function(
  bibliography = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib"),
  csl = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl"),
  render_changes = TRUE,
  ...
) {
  # setup the pdf format
  format <- rmarkdown::latex_document(
    ...,
    toc = FALSE,
    includes = rmarkdown::includes(
      in_header = c(
        system.file("common.sty", package = "dynbenchmark"),
        system.file("manuscript.sty", package = "dynbenchmark")
      )
    ),
    latex_engine = "xelatex",
    number_sections = FALSE
  )

  format <- common_dynbenchmark_format(
    format,
    bibliography = bibliography,
    csl = csl
  )

  # add changes formatter
  # format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_changes))
  format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_header_newline))

  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # render with changes
    if (render_changes) {
      output_file_changes <- paste0(fs::path_ext_remove(output_file), "_changes.tex")
      read_lines(output_file) %>% process_changes(render_changes = TRUE) %>% write_lines(output_file_changes)
      system(glue::glue("xelatex -interaction=nonstopmode {output_file_changes}"))
    }

    # render without changes
    read_lines(output_file) %>% process_changes(render_changes = FALSE) %>% write_lines(output_file)
    system(glue::glue("xelatex -interaction=nonstopmode {output_file}"))

    # save the supplementary figures and tables
    write_rds(lst(sfigs = get("sfigs", envir = .GlobalEnv), stables, refs), "supplementary.rds")

    # return the new output file
    fs::path_ext_set(output_file, "pdf")
  }

  format
}

#' Common dynbenchmark format
#' @param format The format on which to apply common changes
#' @param bibliography Bibliography location
#' @param csl Csl file location
common_dynbenchmark_format <- function(
  format,
  bibliography = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib"),
  csl = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl")
) {
  # allow duplicate labels, needed for nested documents to work
  options(knitr.duplicate.label = 'allow')

  # setup the refs globally
  format$pre_knit <- function(...) {setup_refs_globally()}

  # adapt knitr options
  format$knitr$opts_chunk$echo = FALSE
  format$knitr$opts_chunk$fig.path = ".figures/"

  # activate pandoc citation processing
  format$pandoc$args <- c(
    format$pandoc$args,
    glue::glue("--bibliography={bibliography}"),
    glue::glue("--csl={csl}"),
    "--metadata", "link-citations=true",
    "--metadata", "reference-section-title=References",
    "--filter=/usr/lib/rstudio/bin/pandoc/pandoc-citeproc"
  )

  format
}



#' Knit a child, and add an extra level of headings + fix relative paths. Can be used both for latex and markdown output formats
#'
#' @param file File to knit, can also be a directory in which case the README.Rmd will be knit
#' @export
knit_nest <- function(file) {
  # check if directory -> use README
  if (fs::is_dir(file)) {
    file <- file.path(file, "README.Rmd")
  }
  folder <- fs::path_dir(file) %>% fs::path_rel()

  # stop if file not present
  if (!file.exists(file)) {
    stop(file, " does not exist!")
  }

  # choose between markdown output and latex output
  format <- get_default_format()
  if (format == "markdown") {
    # when markdown, simply include the markdown file, but with some adaptations obviously
    knit <- readr::read_lines(fs::path_ext_set(file, "md"))

    # fix relative paths to links and figures
    knit <- fix_relative_paths(knit, folder)

    # add extra header sublevels & add link
    knit <- knit %>%
      str_replace_all("^(# )(.*)$", paste0("\\1[\\2](", folder, ")")) %>%
      str_replace_all("^#", "##")

    # cat output
    knit %>% glue::glue_collapse("\n") %>% knitr::asis_output()
  } else if (format == "latex") {
    # make sure duplicated labels are allowed
    options(knitr.duplicate.label = "allow")

    # knit as a child
    knitr::knit_child(
      text = readr::read_lines(file) %>% stringr::str_replace_all("^#", "##"),
      quiet = TRUE
    ) %>% knitr::asis_output()
  }
}
