library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

list2env(read_rds(result_file("aggregated_data.rds", "9-main_figure")), environment())

method_tib <- method_tib %>%
  # filter(!method_short_name %in% c("ouija", "scimitar", "pseudogp")) %>%
  arrange(earliest_date) %>%
  mutate(
    leading = cummax(overall) == overall,
    leading_benchmark = cummax(overall_benchmark) == overall_benchmark,
    leading_qc = cummax(overall_qc) == overall_qc
  )

leading <- bind_rows(
  method_tib %>% filter(leading),
  method_tib %>% summarise_if(is.numeric, max, na.rm = T) %>% mutate(earliest_date = Sys.time() %>% as.Date())
)
leading_benchmark <- bind_rows(
  method_tib %>% filter(leading_benchmark),
  method_tib %>% summarise_if(is.numeric, max, na.rm = T) %>% mutate(earliest_date = Sys.time() %>% as.Date())
)
leading_qc <- bind_rows(
  method_tib %>% filter(leading_qc),
  method_tib %>% summarise_if(is.numeric, max, na.rm = T) %>% mutate(earliest_date = Sys.time() %>% as.Date())
)

g1 <- ggplot(method_tib, aes(earliest_date, overall)) +
  geom_step(data = leading, colour = "red") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name), nudge_y = .005) +
  labs(x = "Time", y = "Overall score", title = "Best overall score over time") +
  cowplot::theme_cowplot()

g2 <- ggplot(method_tib, aes(earliest_date, overall_benchmark)) +
  geom_step(data = leading_benchmark, colour = "red") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name), nudge_y = .005) +
  labs(x = "Time", y = "Benchmark score", title = "Best benchmark score over time") +
  cowplot::theme_cowplot()

g3 <- ggplot(method_tib, aes(earliest_date, overall_qc)) +
  geom_step(data = leading_qc, colour = "red") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name), nudge_y = .005) +
  labs(x = "Time", y = "QC score", title = "Best QC score over time") +
  cowplot::theme_cowplot()

ggsave(figure_file("leading_method.svg"), cowplot::plot_grid(g1, g2, g3, ncol = 1), width = 12, height = 15)
