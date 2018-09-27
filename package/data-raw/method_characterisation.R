topinf_types <- tibble(
  name = c("free", "parameter", "fixed"),
  short_name = c("free", "param", "fixed"),
  colour = c("#00ab1b", "#edb600", "#cc2400"),
  explanation = c("inferred by algorithm", "determined by parameter", "fixed by algorithm")
)

error_reasons <- tibble(
  name = c("pct_memory_limit", "pct_time_limit", "pct_method_error_all", "pct_method_error_stoch", "pct_execution_error", "pct_method_error"),
  label = c("Memory limit exceeded", "Time limit exceeded", "Dataset-specific error", "Stochastic error", "Execution error", "Method error"),
  colour = RColorBrewer::brewer.pal(length(name), "Set3")
)
devtools::use_data(error_reasons, topinf_types, overwrite = TRUE, pkg = "package")
