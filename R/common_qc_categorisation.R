#' Applications
#' @export
qc_applications <- tibble(application = c("developer_friendly", "user_friendly", "good_science"))

#' Categories
#' @export
qc_categories <- tibble(
  category = c("availability", "code_quality", "code_assurance", "documentation", "behaviour", "paper"),
  color = c("#3498DB", "#E74C3C", "#A4CC2E", "#FEB308", "#B10DC9", "#85144b", "#EA8F10", "#2ECC49", "#CC2E63")[1:length(category)])
