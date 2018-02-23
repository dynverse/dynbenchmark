library(xml2)

xml_find_multiple_ids <- function(xml, ids) {
  ids_txt <- paste0("@id = \'", ids, "\'", collapse=" or ")
  xpath <- glue("//svg:g[{ids_txt}]")
  xml_find_all(xml, xpath)
}

change_opacity <- function(
  boxes,
  output,
  svg_location,
  folder = ".",
  verbose = FALSE,
  export = "--export-area-page",
  trim = FALSE,
  size = "100%",
  identifier = "g",
  attribute = "id",
  png=TRUE,
  svg=FALSE
) {
  if (!("opacity" %in% colnames(boxes))) {
    stop("Need opacity")
  }

  xml <- read_xml(svg_location)

  xml_find_multiple_ids <- function(xml, ids) {
    ids_txt <- paste0(glue::glue("@{attribute} = \'"), ids, "\'", collapse=" or ")
    xpath <- glue::glue(".//svg:{identifier}[{ids_txt}]")

    print(xpath)
    xml_find_all(xml, xpath)
  }

  walk(boxes %>% split(boxes$opacity), function(x) {
    nodes <- xml_find_multiple_ids(
      xml,
      x$id
    )
    xml_attr(nodes, "style") <- glue::glue("opacity:{x$opacity[[1]]};")

    if (x$opacity[[1]] == 0) {
      xml_remove(nodes)
    }
  })

  write(as.character(xml), file=glue::glue("{folder}/{output}.svg"))

  if(svg && trim) {
    command <- glue::glue("inkscape --verb=FitCanvasToDrawing --verb=FileSave --verb=FileQuit {folder}/{output}.svg")
    system(command, ignore.stdout = !verbose)
  }

  if (!svg) {
    file.remove(glue::glue("{folder}/{output}.svg"))
  }

  if (png) {
    if(trim) {
      system(glue::glue("convert {folder}/{output}.png -trim {folder}/{output}.png"))
    }
    system(glue::glue("inkscape {folder}/{output}.svg --export-area-page --export-png={folder}/{output}.png"), ignore.stdout = !verbose)
  }

  glue::glue('{folder}/{output}.png')

  #glue::glue("<img src='{folder}/{output}.png' style='max-width:{size};max-height:{size}' />")
}



