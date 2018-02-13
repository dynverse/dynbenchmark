library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("manual_ti")

# load in all datasets
tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
tasks <- tasks %>% filter(task_id != "real/dentate-gyrus-neurogenesis_hochgerner")

run <- lst(
  dimred_id = "mds",
  person_id = "robrechtc",
  run_i = 1,
  run_id=glue::collapse(c(dimred_id, person_id, run_i), "_"),
  seed = sodium::data_encrypt(charToRaw(run_id), sodium::hash(charToRaw("robrecht")), nonce = sodium::hash(charToRaw("hi"), size = 24)) %>% rawToBits() %>% head(32) %>% packBits("integer") %>% abs()
)

if(file.exists(derived_file(paste0(run$run_id, ".rds")))) {
  run2 <- read_rds(derived_file(paste0(run$run_id, ".rds")))

  previous_task_ids <- run2$spaces %>% pull(task_id)
  new_task_ids <- tasks$task_id[!(tasks$task_id %in% previous_task_ids)]

  print("Previous run found, using previous ordering (plus extra datasets)")
  selected_tasks <- tasks %>% slice(match(c(previous_task_ids, new_task_ids), task_id))
} else {
  set.seed(run$seed)
  selected_tasks <- tasks %>% arrange(sample(n()))
}

##  ............................................................................
##  Generate svg for run                                                    ####

source("../dynmodular/dimred_wrappers.R")
dimred_function <- get(paste0("dimred_", run$dimred_id))

##  ............................................................................
##  Plot cells                                                              ####

load_expression <- function(expression) {
  if(class(expression) == "matrix") {
    expression
  } else {
    expression()
  }
}

spaces <- pmap(as.list(selected_tasks), function(...) {
  # task <- extract_row_to_list(tasks %>% filter(category == "real"), 21)
  task <- list(...)
  print(task$id)

  task$expression <- load_expression(task$expression)

  plots <- list()

  set.seed(run$seed)
  if (task$category == "control") {
    space <- task$space
  } else {
    space <- dimred_pca(task$expression)
    space <- space %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")
  }

  plot <- ggplot(space) +
    geom_point(aes(Comp1, Comp2), alpha=0.5) +
    ggraph::theme_graph()

  tibble(
    plot=list(plot),
    id = task$id,
    task_id = task$task_id,
    x_scale = max(space$Comp1) - min(space$Comp1),
    y_scale = max(space$Comp2) - min(space$Comp2),
    x_shift = min(space$Comp1),
    y_shift = min(space$Comp2),
    space=list(as.data.frame(space))
  )
}) %>% bind_rows() %>% mutate(box_id = row_number())



##  ............................................................................
##  Create svg                                                              ####

base_size <- 3
plots <- spaces$plot
ncol <- 5
nrow <- ceiling(length(plots)/ncol)
plots %>%
  cowplot::plot_grid(plotlist=., ncol=5) %>%
  cowplot::save_plot(derived_file(glue::glue("{run$run_id}.png")), ., base_width = base_size*ncol, base_height = nrow * base_size, limitsize=F)

svg <- glue::glue(
"
<?xml version='1.0' standalone='no'?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN'
  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>
<svg width='{ncol*base_size}cm' height='{nrow*base_size}cm' version='1.1'
     xmlns='http://www.w3.org/2000/svg' xmlns:xlink= 'http://www.w3.org/1999/xlink'>
<g inkscape:label='back' sodipodi:insensitive='true' inkscape:groupmode='layer'>
<image xlink:href='{run$run_id}.png' x='0' y='0' width='{ncol*base_size}cm' height='{nrow*base_size}cm' />
</g>
<g inkscape:label='segments' inkscape:groupmode='layer'>
</g>

</svg>
")

if(!(run$run_id %in% runs$run_id)) {
  svg %>% xml2::read_xml() %>% xml2::write_xml(derived_file(glue::glue("{run$run_id}.svg")))
} else {
  svg <- xml2::read_xml(derived_file(glue::glue("{run$run_id}.svg")))
  xml2::xml_find_first(svg, ".//svg:image")
  xml2::xml_replace(xml2::xml_find_first(svg, ".//svg:image"), glue::glue("<svg:image xlink:href='{run$run_id}.png' x='0' y='0' width='{ncol*base_size}cm' height='{nrow*base_size}cm' />"), .copy=FALSE)
  xml2::xml_attr(svg, "width") <- glue::glue("{ncol*base_size}cm")
  xml2::xml_attr(svg, "height") <- glue::glue("{nrow*base_size}cm")
  svg %>% xml2::write_xml(derived_file(glue::glue("{run$run_id}_new.svg")))

  print("Not recreating svg")
}


##  ............................................................................
##  Save run info                                                           ####
run$base_size <- base_size
run$ncol <- ncol
run$nrow <- nrow
run$spaces <- list(select(spaces, -plot)) # the plots are enormous (pryr::object_size(spaces$plot))
run %>% write_rds(derived_file(paste0(run$run_id, ".rds")))
