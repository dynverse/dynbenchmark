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
  size = "100%"
) {
  if (!("opacity" %in% colnames(boxes))) {
    stop("Need opacity")
  }

  svg <- read_xml(svg_location)

  xml_find_multiple_ids <- function(xml, ids) {
    ids_txt <- paste0("@id = \'", ids, "\'", collapse=" or ")
    xpath <- glue("//svg:g[{ids_txt}]")
    xml_find_all(xml, xpath)
  }

  walk(boxes %>% split(boxes$opacity), function(x) {
    nodes <- xml_find_multiple_ids(
      svg,
      x$id
    )
    xml_attr(nodes, "style") <- glue("opacity:{x$opacity[[1]]};")
  })

  write(as.character(svg), file=glue::glue("{folder}/{output}.svg"))
  system(glue::glue("inkscape {folder}/{output}.svg --export-area-page --export-png={folder}/{output}.png"), ignore.stdout = !verbose)

  file.remove(glue::glue("{folder}/{output}.svg"))

  if(trim) {
    system(glue::glue("convert {folder}/{output}.png -trim {folder}/{output}.png"))
  }

  glue::glue('{folder}/{output}.png')

  #glue::glue("<img src='{folder}/{output}.png' style='max-width:{size};max-height:{size}' />")
}



boxes <- tibble(
  id = c("linear", "single_bifurcation", "binary_tree", "single_multifurcation", "non_binary_tree", "single_cycle", "simple_graph", "single_bifurcation_single_convergence"), opacity = 1
)
map(boxes$id, function(id) {
  boxes$opacity <- ifelse(boxes$id == id, 1, 0)

  change_opacity(boxes, id, "analysis/figures/trajectory_types.svg", folder = "analysis/figures/trajectory_types/mini/", export = "--export-area-drawing", trim=TRUE)
})

