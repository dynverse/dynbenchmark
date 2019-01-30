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

  format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_header_newline))

  # apply common dynbenchmark format
  format <- common_dynbenchmark_format(format)

  format
}


#' A supplementary note pdf document
#'
#' @inheritParams pdf_manuscript
#'
#' @export
pdf_supplementary_note <- function(
  ...
) {
  pdf_manuscript(
    ...,
    render_changes = FALSE,
    reference_section_title = "Supplementary References"
  )
}



#' The manuscript pdf document
#'
#' @inheritParams common_dynbenchmark_format
#' @param render_changes Whether to export a *_changes.pdf as well
#' @param toc Whether to include a table of contents
#' @param linenumbers Whether to add linenumbers to the left side
#' @param sty_header Extra sty files to be included in the header of the document
#' @param sty_beforebody Extra sty files to be included before the body of the document
#' @param sty_afterbody Extra sty files to be included after the body of the document
#' @param ... Parameters for rmarkdown::pdf_document
#'
#' @export
pdf_manuscript <- function(
  bibliography = raw_file("references.bib", "12-manuscript"),
  csl = raw_file("nature-biotechnology.csl", "12-manuscript"),
  render_changes = TRUE,
  toc = FALSE,
  linenumbers = FALSE,
  sty_header = NULL,
  sty_beforebody = NULL,
  sty_afterbody = NULL,
  reference_section_title = reference_section_title,
  ...
) {
  header_includes <- c(
    raw_file("common.sty", "12-manuscript"),
    raw_file("manuscript.sty", "12-manuscript"),
    sty_header
  )

  if (linenumbers) {
    header_includes <- c(
      header_includes,
      raw_file("linenumbers.sty", "12-manuscript")
    )
  }
  # setup the pdf format
  format <- rmarkdown::latex_document(
    ...,
    toc = toc,
    includes = rmarkdown::includes(
      in_header = header_includes,
      before_body = sty_beforebody,
      after_body = sty_afterbody
    ),
    latex_engine = "xelatex",
    number_sections = FALSE
  )

  format <- common_dynbenchmark_format(
    format,
    bibliography = bibliography,
    csl = csl,
    reference_section_title = reference_section_title
  )

  # add changes formatter
  # format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_changes))
  format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_header_newline))

  format$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    # render with changes
    if (render_changes) {
      output_file_changes <- paste0(fs::path_ext_remove(output_file), "_changes.tex")
      read_lines(output_file) %>% process_changes(render_changes = TRUE) %>% write_lines(output_file_changes)
      system(glue::glue("xelatex -interaction=nonstopmode -output-directory={fs::path_dir(output_file)} {output_file_changes}"))
      # clean up files if requested
      if (clean) clean_xelatex(output_file_changes)
    }

    # render without changes
    read_lines(output_file) %>% process_changes(render_changes = FALSE) %>% write_lines(output_file)
    system(glue::glue("xelatex -interaction=nonstopmode -output-directory={fs::path_dir(output_file)} {output_file}"))

    if (toc) {
      system(glue::glue("xelatex -interaction=nonstopmode -output-directory={fs::path_dir(output_file)} {output_file}"))
    }

    # clean up files if requested
    if (clean) clean_xelatex(output_file)

    # save the supplementary figures and tables
    write_rds(lst(sfigs = get("sfigs", envir = .GlobalEnv), stables, refs), "supplementary.rds")

    # return the new output file
    fs::path_ext_set(output_file, "pdf")
  }

  format
}


clean_xelatex <- function(output_file) {
  fs::file_delete(fs::path_ext_set(output_file, "log"))
  fs::file_delete(fs::path_ext_set(output_file, "tex"))
  fs::file_delete(fs::path_ext_set(output_file, "aux"))
  fs::file_delete(fs::path_ext_set(output_file, "out"))
  if(fs::file_exists(fs::path_ext_set(output_file, "toc"))) fs::file_delete(fs::path_ext_set(output_file, "toc"))
}

#' Common dynbenchmark format
#' @param format The format on which to apply common changes
#' @param bibliography Bibliography location
#' @param csl Csl file location
#' @param reference_section_title The title of the reference section
common_dynbenchmark_format <- function(
  format,
  bibliography = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib"),
  csl = paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl"),
  reference_section_title = "References"
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
    "--metadata", paste0("reference-section-title=", reference_section_title),
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
  # get absolute folder based on current working directory of knitr
  file <- fs::path(knitr::opts_chunk$get("root.dir") %||% ".", file)
  folder <- fs::path_dir(file) %>% fs::path_abs()

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
      str_replace_all("^(# )(.*)$", paste0("\\1[\\2](", fs::path_rel(folder), ")")) %>%
      str_replace_all("^#", "##")

    # cat output
    knit %>% glue::glue_collapse("\n") %>% knitr::asis_output()
  } else if (format == "latex") {
    # make sure duplicated labels are allowed
    options(knitr.duplicate.label = "allow")

    # make sure to run inside new folder
    # knitr::opts_chunk$set(root.dir = folder)

    # knit as a child
    knitr::knit_child(
      text = readr::read_lines(file) %>% stringr::str_replace_all("^#", "##"),
      options = list(root.dir = folder),
      quiet = TRUE
    ) %>% knitr::asis_output()

    # knitr::opts_chunk$set(root.dir = folder)
  }
}



#' A nested github markdown document
#'
#' @inheritParams common_dynbenchmark_format
#' @param ... Parameters for rmarkdown::github_document
#'
#' @export
word_manuscript <- function(
  bibliography = fs::path_abs(paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/references.bib")),
  csl = fs::path_abs(paste0(dynbenchmark::get_dynbenchmark_folder(), "manuscript/assets/nature-biotechnology.csl")),
  ...
) {
  format <- rmarkdown::word_document(...)

  format$pre_processor <- append_pre_processor(format, apply_pre_processor(process_header_newline))

  # apply common dynbenchmark format
  format <- common_dynbenchmark_format(format)

  format
}
