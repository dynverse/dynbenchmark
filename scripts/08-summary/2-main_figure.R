library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("08-summary")

####################################
###       PREP DATA TIBBLE       ###
####################################
method_groups <- c(rev(dynwrap::trajectory_types$id), c("Adaptation", "Off-the-shelf", "Control"))

wrapper_type_map <- c(branch_trajectory = "Traj", linear_trajectory = "Linear", cyclic_trajectory = "Cycle", trajectory = "Traj", cell_graph = "Cell", cluster_graph = "Cluster", control = "", dimred_projection = "Proj", end_state_probabilities = "Prob")

data <-
  read_rds(result_file("results.rds", experiment_id = "08-summary")) %>%
  rename(id = method_id) %>%
  mutate(
    group = case_when(
      method_most_complex_trajectory_type %in% c("disconnected_graph", "connected_graph") ~ "graph",
      TRUE ~ method_most_complex_trajectory_type
    ),
    group = factor(group, levels = method_groups),
    method_priors_required_str = case_when(
      grepl("dataset", method_required_priors_str) ~ "All",
      grepl("(groups_id|features_id|timecourse_continuous|timecourse_discrete|groups_network)", method_required_priors_str) ~ "\u2716",
      grepl("(start_id|end_id|end_n|start_n|groups_n)", method_required_priors_str) ~ "\u2715",
      TRUE ~ ""
    ),
    method_topology_inference = label_short(ifelse(method_topology_inference == "parameter", "param", method_topology_inference)),
    method_wrapper_type = wrapper_type_map[method_wrapper_type],
    benchmark_overall_error_reasons = pmap(
      lst(
        "Method error" = benchmark_overall_pct_method_error_all + benchmark_overall_pct_method_error_stoch,
        "Time limit exceeded" = benchmark_overall_pct_time_limit,
        "Memory limit exceeded" = benchmark_overall_pct_memory_limit,
        "Execution error" = benchmark_overall_pct_execution_error
      ),
      c
    ),
    benchmark_overall_pct_errored_str = case_when(
      benchmark_overall_pct_errored < .00001 ~ "0%",
      benchmark_overall_pct_errored < .01 ~ "<1%",
      TRUE ~ paste0(round(benchmark_overall_pct_errored * 100), "%")
    )
  ) %>%
  arrange(group, desc(summary_overall_overall))

for (col in stringr::str_subset(colnames(data), "^scaling_pred_timestr_")) {
  col_score <- gsub("timestr", "scoretime", col)
  col_comb <- gsub("timestr", "timecomb", col)
  data[[col_comb]] <- map2(data[[col]], data[[col_score]], function(l, v) list(value = v, label = l))
}
for (col in stringr::str_subset(colnames(data), "^scaling_pred_memstr_")) {
  col_score <- gsub("memstr", "scoremem", col)
  col_comb <- gsub("memstr", "memcomb", col)
  data[[col_comb]] <- map2(data[[col]], data[[col_score]], function(l, v) list(value = v, label = l))
}

for (tt in dynwrap::trajectory_types$id) {
  data[[paste0("itt_", tt)]] <- ifelse(data[[paste0("method_detects_", tt)]], tt, paste0("gray_", tt))
}

####################################
###  DETERMINE METHOD GROUPING   ###
####################################
row_info <-
  data %>%
  select(group, id)
row_groups <-
  data %>%
  transmute(Group = paste0(label_short(group), " methods"), group) %>%
  unique()

####################################
###   DETERMINE COLUMN GROUPING  ###
####################################

palettes <- tribble(
  ~palette,        ~colours,
  # blues palette
  "overall", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greys")[-1]))(101),
  "benchmark", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Blues") %>% c("#011636")))(101),
  "scaling", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Reds")[-8:-9]))(101),
  "stability", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlOrBr")[-7:-9]))(101),
  "qc", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greens")[-1] %>% c("#00250f")))(101),

  "error_reasons", error_reasons %>% select(label, colour) %>% deframe(),
  "white6black4", c(rep("white", 4), rep("black", 6)),
  "column_annotation", c(overall = "#555555", benchmark = "#4292c6", scaling = "#f6483a", stability = "#fe9929", qc = "#41ab5d")
)

####################################
###        CREATE FIGURES        ###
####################################
script_files <- c("suppfig", "all", "summary", "detailed")
# script_files <- "suppfig"
# script_files <- c("summary", "detailed")

walk(script_files, function(name) {
  cat("Processing ", name, "\n", sep = "")
  script_file <- scripts_file(c("2a_columns_", name, ".R"))
  plot_file <- result_file(c("results_", name, ".pdf"))

  reformat_tribbles(script_file)
  source(script_file, local = TRUE)

  g <- funky_heatmap(data = data, column_info = column_info, column_groups = column_groups, row_info = row_info, row_groups = row_groups, palettes = palettes, col_annot_offset = 3)

  ggsave(plot_file, g, device = cairo_pdf, width = g$width/4, height = g$height/4)
})


