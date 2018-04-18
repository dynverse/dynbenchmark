source("scripts/0_common.R")

# Delete and recreate folders ----------------
restart <- FALSE

if (restart) {
  unlink(dataset_file(), recursive=TRUE);dir.create(dataset_file(), recursive=TRUE, showWarnings = FALSE)
  unlink(dataset_preproc_file(), recursive=TRUE);dir.create(dataset_preproc_file(), recursive=TRUE, showWarnings = FALSE)
  PRISM:::run_remote(glue::glue("rm -r {paste0(remote_folder, dataset_file(relative = TRUE))}"), "prism")
  PRISM:::run_remote(glue::glue("rm -r {paste0(remote_folder, dataset_preproc_file(relative = TRUE))}"), "prism")
  PRISM:::run_remote(glue::glue("mkdir {paste0(remote_folder, dataset_preproc_file(relative = TRUE))}"), "prism")
  PRISM:::run_remote(glue::glue("mkdir {paste0(remote_folder, dataset_file(relative = TRUE))}"), "prism")
}

# Generate settings ----------------------------
settings_model <- tribble(
  ~modulenet_name, ~totaltime,
  "linear", 5,
  "bifurcating", 5,
  "linear_long", 30,
  "cycle", 30,
  "consecutive_bifurcating", 10,
  "bifurcating_converging", 15,
  "trifurcating", 10,
  "converging", 10,
  "bifurcating_loop", 30
)
settings_platform <- tibble(platform_name = list.files("ext_data/platforms/") %>% gsub("(.*)\\.rds", "\\1", .)) %>% filter(row_number() <= 10) %>% bind_rows(tibble(platform_name = "small"), .)

settings_replicates <- tibble(replicate_id = 1)
settings <- tidyr::crossing(settings_model, settings_platform, settings_replicates)
settings <- settings %>% 
  group_by(modulenet_name) %>% 
  mutate(dataset_id = paste0(modulenet_name, "_", seq_len(n()))) %>% 
  ungroup() %>% 
  mutate(params_i = seq_len(n()))

settings <- settings[1, ]

# create params ----------------------------------
update_params <- function(base_params=dyngen:::base_params, ...) {
  dots <- list(...)
  
  if("modulenet_name" %in% names(dots)) base_params$model$modulenet_name <- dots$modulenet_name
  if("totaltime" %in% names(dots)) base_params$simulation$totaltime <- dots$totaltime
  if("platform_name" %in% names(dots)) {
    base_params$model$platform <- readRDS(paste0(find.package("dyngen"), "/ext_data/platforms/", dots$platform_name, ".rds"))
    base_params$experiment$platform <- readRDS(paste0(find.package("dyngen"), "/ext_data/platforms/", dots$platform_name, ".rds"))
  }
  
  base_params$settings <- dots
  
  base_params
}

paramsets <- map(seq_len(nrow(settings)), function(row_id) {
  row <- dynutils::extract_row_to_list(settings, row_id)
  invoke(update_params, row)
})

# save paramsets ----------------------------------------
saveRDS(paramsets, dataset_preproc_file("paramsets.rds"))
PRISM:::rsync_remote("", dataset_preproc_file(), "prism", paste0(remote_folder, dataset_preproc_file(relative=TRUE)))
