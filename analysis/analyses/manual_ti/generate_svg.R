library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("manual_ti")

# load in all datasets
tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation")) %>% filter() %>% filter(category != "toy")

# runs <- tibble(dimred_id = character(), person_id = character(), run_i = integer(), run_id = character(), seed = integer()) %>% write_rds(result_file("runs.rds"))
runs <- read_rds(result_file("runs.rds"))

run <- lst(
  dimred_id = "pca",
  person_id = "wouters",
  run_i = 1,
  run_id=glue::collapse(c(dimred_id, person_id, run_i), "_"),
  seed = sodium::data_encrypt(charToRaw(run_id), sodium::hash(charToRaw("robrecht")), nonce = sodium::hash(charToRaw("hi"), size = 24)) %>% rawToBits() %>% head(32) %>% packBits("integer") %>% abs()
) %>% list %>% dynutils::list_as_tibble()

set.seed(run$seed)
selected_tasks <- tasks %>% arrange(sample(n()))

run$task_ids <- list(tasks$id)

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
    task_id = task$id,
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
svg %>% xml2::read_xml() %>% xml2::write_xml(derived_file(glue::glue("{run$run_id}.svg")))



##  ............................................................................
##  Save run info                                                           ####
run$base_size <- base_size
run$ncol <- ncol
run$nrow <- nrow
run$spaces <- list(select(spaces, -plot)) # the plots are enormous (pryr::object_size(spaces$plot))
runs <- runs %>% bind_rows(run) %>% group_by(run_id) %>% filter(row_number() == n()) %>% ungroup()
write_rds(runs, derived_file("runs.rds"))
