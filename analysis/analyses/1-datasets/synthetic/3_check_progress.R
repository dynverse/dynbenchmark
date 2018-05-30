run_qstat <- function() {
  out_qhost <- PRISM:::run_remote("qhost -F", "prism")$cmd_out
  out_qstat <- PRISM:::run_remote("qstat -u \"*\" -f", "prism")$cmd_out
  
  processed_qstat_string <- paste0(paste(out_qstat[grepl("^  ", out_qstat)], collapse="\n"), "\n\n")
  
  task_id_fun <- function(x) {
    if (grepl("-", x)) {
      prms <- as.integer(strsplit(x, "[-:]")[[1]])
      (prms[[2]] - prms[[1]]) / prms[[3]] + 1
    } else if (grepl(",", x)) {
      strsplit(x, ",")[[1]] %>% length
    } else {
      1
    }
  }
  
  split_task_id <- function(x) {
    map(x, function(x) {
      gsub(":[0-9]*", "", x) %>% gsub("-", ":", .) %>% parse(text=.) %>% eval()
    })
  }
  
  if (processed_qstat_string != "\n\n") {
    qstat_tab <- readr::read_table(processed_qstat_string, col_names = c("job_id", "prior", "name", "user", "state", "date", "queue", "slots", "task_id")) %>% mutate(
      num_tasks = sapply(task_id, task_id_fun),
      total_slots = slots * num_tasks,
      running = grepl("r", state),
      error = grepl("E", state),
      queued = grepl("q", state) & !error,
      task_ids = split_task_id(task_id)
    )
  } else {
    qstat_tab <- data_frame(job_id = character(0), name = character(0), user = character(0), state = character(0), task_ids = numeric())
  }
  
  if (nrow(qstat_tab) > 0) {
    qstat_tab %>% filter(map_int(task_ids, length) > 0) %>% unnest(task_ids) %>% mutate(task_id = task_ids) %>% select(-task_ids)
  } else {
    qstat_tab %>% mutate(task_id = task_ids) %>% select(-task_ids)
  }
}



#### 
qstat <- run_qstat()
files <- PRISM:::run_remote(glue::glue("ls {paste0(remote_folder, dataset_preproc_file(relative = TRUE))}"), "prism")$cmd_out

names <- c("model", "simulation", "gs", "gs_plot", "experiment", "experiment_plot", "normalisation", "normalisation_plot", "task")
design <- crossing(task_id = seq_along(paramsets), name = names)
design$status <- design %>% as.list() %>% pmap(function(task_id, name) {
  name <- as.character(name)
  prism_names <- setNames(names, names)
  prism_names[["normalisation"]] <- "normalisat"
  prism_names[["task"]] <- "wrap"
  prism_names[["experiment_plot"]] <- "exp_plot"
  if (qstat %>% filter(prism_names[!!name] == name, task_id == !!task_id, state == "r") %>% nrow()) {
    "running"
  } else if (qstat %>% filter(prism_names[!!name] == name, task_id == !!task_id, state == "qw") %>% nrow()) {
    "waiting"
  } else if (any(str_detect(files, glue::glue("^{task_id}_{name}\\..*")))) {
    "output"
  } else {
    "not_present"
  }
}) %>% unlist()
design %>% 
  mutate(name = factor(name, levels=names)) %>% 
  ggplot(aes(name, as.character(task_id))) + 
  geom_tile(aes(fill=status)) + 
  scale_fill_manual(values=c(output="#0074D9", running="#2ECC40", waiting="#FF851B", "not_present"="#FF4136")) + 
  geom_text(aes(label=task_id), color="white", size=2) +
  cowplot::theme_cowplot() +
  scale_x_discrete(expand=c(0,0))  + scale_y_discrete(expand=c(0,0))
