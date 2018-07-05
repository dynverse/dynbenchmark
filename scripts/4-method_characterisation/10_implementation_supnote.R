library(googlesheets)
library(tidyverse)
library(dynbenchmark)

experiment("4-method_characterisation")

methods <- read_rds(derived_file("methods_evaluated.rds"))
methods$run_fun <- paste0("ti_", methods$method_id) %>% map(get, "package:dynmethods") %>% map(~.()) %>% map("run_fun")

methods$ti_methods <- paste0("ti_", methods$method_id) %>% map(get, "package:dynmethods") %>% map(~.())
methods$inputs <- methods$ti_methods %>% map("run_fun") %>% map(formals)
methods$par_set <- methods$ti_methods %>% map("par_set")

get_input_expression <- function(inputs) {
  possible_inputs <- c("expression", "counts", "dataset")
  intersect(names(inputs), possible_inputs) %>% first()
}
methods$input_expression <- map_chr(methods$inputs, get_input_expression)

param_print_functions <- list(
  discrete = function(x) {
    glue::collapse(x$values, ", ")
  },
  integer = function(x) {
    seq(x$lower, x$upper) %>% glue::collapse(", ") %>% str_replace("([0-9]*, [0-9]*, )[0-9, ]*(, [0-9]*)", "\\1...\\2")
  },
  numeric = function(x) {
    glue::glue("\\[{x$lower}, {x$upper}\\]")
  }
)


create_detectable_trajectory_types_table <- function(implementation) {
  trajectory_types %>%
    filter(directedness == "undirected") %>%

}


create_method_table <- function(implementation) {

}


