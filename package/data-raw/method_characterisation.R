topinf_types <- tibble(
  name = c("free", "parameter", "fixed"),
  short_name = c("free", "param", "fixed"),
  colour = c("#00ab1b", "#edb600", "#cc2400"),
  explanation = c("inferred by algorithm", "determined by parameter", "fixed by algorithm")
)

error_reasons <- tibble(
  name = c("pct_memory_limit", "pct_time_limit", "pct_execution_error", "pct_method_error"),
  label = c("Memory limit exceeded", "Time limit exceeded", "Execution error", "Method error"),
  colour = RColorBrewer::brewer.pal(length(name), "Set3")
)
usethis::proj_set("package")
usethis::use_data(error_reasons = error_reasons, topinf_types = topinf_types, overwrite = TRUE)
