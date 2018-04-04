library(tidyverse)
library(dynalysis)

experiment("trajectory_types")

## Helper functions
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
  size = "100%",
  identifier = "g",
  attribute = "id"
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

  command <- glue::glue("inkscape --verb=FitCanvasToDrawing --verb=FileVacuum --verb=FileSave --verb=FileQuit {folder}/{output}.svg")
  system(command, ignore.stdout = !verbose)


  glue::glue('{folder}/{output}.svg')
}




# Check traj types
trajectory_types$id

boxes <- tibble(
  id = trajectory_types$id, opacity = 1
)
map(boxes$id, function(id) {
  boxes$opacity <- ifelse(boxes$id == id, 1, 0)

  change_opacity(boxes, id, figure_file("trajectory_types.svg"), folder = figure_file("mini"))
})

boxes$base64 <- boxes$id %>% map_chr(function(x) {base64enc::base64encode(glue::glue("analysis/figures/trajectory_types/mini/{x}.svg"))})

write_rds(boxes, figure_file("trajectory_type_boxes.rds"))

