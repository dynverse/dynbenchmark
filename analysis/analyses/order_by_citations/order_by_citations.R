library(tidyverse)
library(googlesheets)
library(dynalysis)

experiment(
  dirname = "TI_method_spreadsheet",
  description = "Testing whether each method is able to run on the cluster",
  auto_create_folders = TRUE
)

# # If it's your first time running this script, run this:
# gs_auth()

script_file <- "analysis/analyses/order_by_citations/scholar.py"
if (!file.exists(script_file)) {
  download.file("https://raw.githubusercontent.com/ckreibich/scholar.py/master/scholar.py", destfile = script_file)
}

num_citations_by_clusterid <- function(clusterid, scholar_file = script_file) {
  tryCatch({
    command <- paste0("python ", scholar_file, " -C ", clusterid, " --csv-header")
    output <- system(command, intern = T)
    tab <- readr::read_delim(paste(gsub("\n", " ", output), collapse = "\n"), delim = "|")
    sum(tab$num_citations)
  }, error = function(e) NA)
}

method_df <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Software", col_types = cols(GScholarClusterID = "c"), skip = 1) %>%
  filter(!is.na(qc_score))

method_df$date <- method_df$Preprint
method_df$date[is.na(method_df$date)] <- method_df$PubDate[is.na(method_df$date)]

method_df <- method_df %>%
  mutate(citations = pbapply::pbsapply(GScholarClusterID, num_citations_by_clusterid))

method_df %>% select(name, citations) %>% as.data.frame

g1 <- ggplot(method_df, aes(date, qc_score)) +
  geom_smooth() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
g1
ggsave(figure_file("time_vs_qcscore.pdf"), g1, width = 16, height = 8)

g2 <- ggplot(method_df, aes(date, Citations+1)) +
  geom_smooth() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_log10() +
  labs(x = "Time", y = "Citations", title = "Citations over time")
g2
ggsave(figure_file("time_vs_citations.pdf"), g2, width = 16, height = 8)


g3 <- ggplot(method_df, aes(Citations+1, qc_score)) +
  geom_smooth() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "QC score", title = "QC score over # citations")
g3
ggsave(figure_file("citations_vs_qcscore.pdf"), g3, width = 8, height = 8)
