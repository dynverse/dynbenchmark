run_qstat <- function() {
  out_qhost <- qsub:::run_remote("qhost -F", "prism")$cmd_out
  out_qstat <- qsub:::run_remote("qstat -u \"*\" -f", "prism")$cmd_out
  
  processed_qstat_string <- paste0(paste(out_qstat[grepl("^  ", out_qstat)], collapse = "\n"), "\n\n")
  
  dataset_id_fun <- function(x) {
    if (grepl("-", x)) {
      prms <- as.integer(strsplit(x, "[-:]")[[1]])
      (prms[[2]] - prms[[1]]) / prms[[3]] + 1
    } else if (grepl(",", x)) {
      strsplit(x, ",")[[1]] %>% length
    } else {
      1
    }
  }
  
  split_dataset_id <- function(x) {
    map(x, function(x) {
      gsub(":[0-9]*", "", x) %>% gsub("-", ":", .) %>% parse(text = .) %>% eval()
    })
  }
  
  if (processed_qstat_string != "\n\n") {
    qstat_tab <- readr::read_table(processed_qstat_string, col_names = c("job_id", "prior", "name", "user", "state", "date", "queue", "slots", "dataset_id")) %>% mutate(
      num_datasets = sapply(dataset_id, dataset_id_fun),
      total_slots = slots * num_datasets,
      running = grepl("r", state),
      error = grepl("E", state),
      queued = grepl("q", state) & !error,
      dataset_ids = split_dataset_id(dataset_id)
    )
  } else {
    qstat_tab <- data_frame(job_id = character(0), name = character(0), user = character(0), state = character(0), dataset_ids = numeric())
  }
  
  if (nrow(qstat_tab) > 0) {
    qstat_tab %>% filter(map_int(dataset_ids, length) > 0) %>% unnest(dataset_ids) %>% mutate(dataset_id = dataset_ids) %>% select(-dataset_ids)
  } else {
    qstat_tab %>% mutate(dataset_id = dataset_ids) %>% select(-dataset_ids)
  }
}



#### 
qstat <- run_qstat()
files <- qsub:::run_remote(glue::glue("ls {paste0(remote_folder, dataset_source_file(relative = TRUE))}"), "prism")$cmd_out

names <- c("model", "simulation", "gs", "gs_plot", "experiment", "experiment_plot", "normalisation", "normalisation_plot", "dataset")
design <- crossing(dataset_id = seq_along(paramsets), name = names)
design$status <- design %>% as.list() %>% pmap(function(dataset_id, name) {
  name <- as.character(name)
  prism_names <- setNames(names, names)
  prism_names[["normalisation"]] <- "normalisat"
  prism_names[["dataset"]] <- "wrap"
  prism_names[["experiment_plot"]] <- "exp_plot"
  if (qstat %>% filter(prism_names[!!name] == name, dataset_id == !!dataset_id, state == "r") %>% nrow()) {
    "running"
  } else if (qstat %>% filter(prism_names[!!name] == name, dataset_id == !!dataset_id, state == "qw") %>% nrow()) {
    "waiting"
  } else if (any(str_detect(files, glue::glue("^{dataset_id}_{name}\\..*")))) {
    "output"
  } else {
    "not_present"
  }
}) %>% unlist()
design %>% 
  mutate(name = factor(name, levels = names)) %>% 
  ggplot(aes(name, as.character(dataset_id))) + 
  geom_tile(aes(fill = status)) + 
  scale_fill_manual(values = c(output = "#0074D9", running = "#2ECC40", waiting = "#FF851B", "not_present" = "#FF4136")) + 
  geom_text(aes(label = dataset_id), color = "white", size = 2) +
  cowplot::theme_cowplot() +
  scale_x_discrete(expand = c(0,0))  + scale_y_discrete(expand = c(0,0))
