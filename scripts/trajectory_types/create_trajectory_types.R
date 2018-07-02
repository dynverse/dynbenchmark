library(tidyverse)
library(dynverse)

experiment("trajectory_types")

## Helper functions
library(xml2)

xml_find_multiple_ids <- function(xml, ids) {
  ids_txt <- paste0(glue::glue("@id = \'"), ids, "\'", collapse = " or ")
  xpath <- glue::glue(".//svg:g[{ids_txt}]")

  xml_find_all(xml, xpath)
}

xml <- read_xml(figure_file("trajectory_types.svg"))

# Check traj types are in the xml
trajtype_ids <- xml %>% xml_find_multiple_ids(trajectory_types$id) %>% xml_attr("id")

walk(trajtype_ids, function(id) {
  mini_name <- figure_file(glue::glue("mini/{id}.svg"))

  xml_mini <- read_xml(figure_file("trajectory_types.svg"))

  remove_ids <- setdiff(trajtype_ids, id) %>% c("layer2", "layer3")

  xml_mini %>%
    xml_find_multiple_ids(remove_ids) %>%
    xml_remove()

  write(as.character(xml_mini), file = mini_name)

  command <- glue::glue("inkscape --verb = FitCanvasToDrawing --verb = FileVacuum --verb = FileSave --verb = FileQuit {mini_name}")
  system(command, ignore.stdout = F)
})
