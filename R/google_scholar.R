#' Find the number of citations for a given google scholar id
#'
#' @param cluster_id The Google Scholar cluster id to query
#'
#' @export
#'
#' @importFrom httr GET add_headers
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom stats runif
google_scholar <- function(cluster_id) {
  base_url <- "https://scholar.google.com/scholar"

  Sys.sleep(stats::runif(1, 0.05, .1))

  random_user_agent <- paste0(sample(c("Mozilla", "Chrome"), 1), "/", sample.int(100, 1), ".", sample.int(100, 1))

  site_html <- httr::GET(base_url, query = list(hl = "en", cluster = cluster_id, pagesize = "100", view_op = "list_works", sortby = "pubdate"), httr::add_headers("user_agent" = random_user_agent))
  webpage <- xml2::read_html(site_html)

  results <- webpage %>% html_nodes(".gs_ri")

  map_df(results, function(result) {
    title <- result %>%
      html_nodes(".gs_rt") %>%
      html_text() %>%
      str_replace_all("\\[[^\\]]*\\] *", "")
    url <- result %>%
      html_nodes(".gs_rt a") %>%
      html_attr(name = "href")
    text_fields <- result %>%
      html_nodes(".gs_fl a") %>%
      html_text()
    num_citations <- text_fields[grepl("Cited by", text_fields)] %>%
      str_replace("Cited by ", "") %>%
      as.integer() %>%
      ifelse(length(.) == 0, NA, .)
    web_of_science <- text_fields[grepl("Web of Science", text_fields)] %>%
      str_replace("Web of Science: ", "") %>%
      as.integer() %>%
      ifelse(length(.) == 0, NA, .)

    data_frame(
      cluster_id,
      title,
      url,
      num_citations,
      web_of_science
    )
  })
}

#' Find the number of citations for a given google scholar id
#'
#' @param cluster_id The Google Scholar cluster id to query
#'
#' @export
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
google_scholar_num_citations <- function(cluster_id) {
  df <- dynutils::google_scholar(cluster_id)
  if (nrow(df) > 0) {
    max(c(0, df$num_citations), na.rm = T)
  } else {
    NA
  }
}
