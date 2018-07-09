# FLASK_APP=server.py FLASK_DEBUG=1 flask run --host=0.0.0.0


library(tidyverse)
library(dynbenchmark)

analysis_folder <- "analysis/analyses/04-method_characterisation"
people <- readr::read_csv(paste0(analysis_folder, "/survey/people.csv"))





generate_person_url <- function(id, people, base_url = "0.0.0.0:5000") {
  key <- people %>% filter(id == !!id) %>% pull(key)

  if (length(key) != 1) stop("Multiple keys or no keys found")
  glue::glue("{base_url}/{key}")
}
