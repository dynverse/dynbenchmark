library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

source("analysis/analyses/4-method_characterization/0_common.R")

## PLOT1
method_characteristics <- read_rds(figure_file("method_characteristics.rds"))
method_qc_ordering_plot <- read_rds(figure_file("method_qc_ordering_plot.rds"))

figure <- plot_grid(
  method_characteristics,
  method_qc_ordering_plot,
  ncol=2,
  labels=c("", "e"),
  rel_widths = c(0.4, 0.6)
)
figure
save_plot(figure_file("figure_methods.pdf"), figure, base_width = 17, base_height= 10)




# replace trajectory types
library(xml2)

command <- glue::glue("inkscape {figure_file('figure_methods.pdf')} --export-plain-svg={figure_file('figure_methods.svg')}")
system(command)

file.remove(figure_file('figure_methods.pdf'))

svg_location <- figure_file('figure_methods.svg')
xml <- read_xml(svg_location)

aspect <- read_xml("analysis/figures/trajectory_types/mini/complex_fork.svg") %>% xml_root() %>% xml_attr("viewBox") %>% str_split(" ") %>% unlist() %>% tail(2) %>% as.numeric() %>% {.[[1]]/.[[2]]}

w <- 50
h <- w / aspect

trajectory_type_boxes <- readRDS(figure_file("trajectory_type_boxes.rds", "trajectory_types"))

map(trajectory_types, function(trajectory_type) {
  to_replace <- xml_find_all(xml, glue::glue("//svg:tspan[text()='{trajectory_type}']")) %>% xml_parent()
  transforms <- to_replace %>% xml_attr("transform")
  images <- map(transforms, function(transform) {
    node <- read_xml(glue::glue("<g><image /></g>"))
    node %>% xml_set_attr("transform", transform)
    node %>% xml_child() %>% xml_set_attrs(list(width=w, height=h, `xlink:href`=paste0("data:image/svg+xml;base64,", trajectory_type_boxes %>% filter(id == trajectory_type) %>% pull(base64)), transform=glue::glue("translate(-{w/2} -{h/2})")))
    node
  })
  replaced <- to_replace %>% xml_replace(images)
  if (length(replaced) == 0) {
    warning("STOOOOOOOOOOOOOOOOOOOOOOOOOOOPPP!!!")
  }
})

xml_root(xml) %>% xml_set_attr("xmlns:xlink", "http://www.w3.org/1999/xlink")

write(as.character(xml), file=figure_file('figure_methods.svg')); xml <- NULL
