library(zenodo)

experiment("2-dataset_characterisation/publish")

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation")) %>% filter(task_source %in% c("real", "synthetic"))
load <- function(x) {
  if(class(x) == "function") {
    x()
  } else {
    x
  }
}

tasks %>% pmap(function(...) {
  task <- list(...)
  task <- map(task, load)
  location <- derived_file(paste0(task$id, ".rds"))
  print(location)
  dir.create(dirname(location), showWarnings = F)
  write_rds(task, location)
  ""
})

system(glue::glue("cd {derived_file()}; zip -r zip.zip ."))
