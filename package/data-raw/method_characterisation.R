topinf_types <- tibble(
  name = c("free", "parameter", "fixed"),
  short_name = c("free", "param", "fixed"),
  colour = c("#00ab1b", "#edb600", "#cc2400"),
  explanation = c("inferred by algorithm", "determined by parameter", "fixed by algorithm")
)
error_reasons <- tibble(
  name = c("pct_memory_exceeded", "pct_time_exceeded", "pct_allerrored", "pct_stochastic"),
  label = c("Memory limit exceeded", "Time limit exceeded", "Dataset-specific error", "Stochastic error"),
  colour = RColorBrewer::brewer.pal(4, "Set3")
)
devtools::use_data(error_reasons, topinf_types, overwrite = TRUE)
