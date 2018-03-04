library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/9-main_figure")

list2env(read_rds(result_file("aggregated_data.rds", "9-main_figure")), environment())

method_tib <- method_tib %>%
  filter(!method_short_name %in% c("ouija", "scimitar", "pseudogp")) %>%
  arrange(earliest_date) %>%
  mutate(
    leading_harm_mean = cummax(harm_mean) == harm_mean,
    leading_qc_score = cummax(qc_score) == qc_score
  )

leading_harm_mean <- bind_rows(
  method_tib %>% filter(leading_harm_mean),
  method_tib %>% summarise_if(is.numeric, max, na.rm = T) %>% mutate(earliest_date = Sys.time() %>% as.Date())
)
leading_qc_score <- bind_rows(
  method_tib %>% filter(leading_qc_score),
  method_tib %>% summarise_if(is.numeric, max, na.rm = T) %>% mutate(earliest_date = Sys.time() %>% as.Date())
)

g1 <- ggplot(method_tib, aes(earliest_date, harm_mean)) +
  geom_step(data = leading_harm_mean, colour = "red") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name), nudge_y = .005) +
  labs(x = "Time", y = "Benchmark score") +
  cowplot::theme_cowplot()

g2 <- ggplot(method_tib, aes(earliest_date, qc_score)) +
  geom_step(data = leading_qc_score, colour = "red") +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name), nudge_y = .005) +
  labs(x = "Time", y = "QC score") +
  cowplot::theme_cowplot()


cowplot::plot_grid(g1, g2, ncol = 1)
ggsave(figure_file("leading_method.svg"), g1, width = 10, height = 6)
ggsave(figure_file("leading_method2.svg"), cowplot::plot_grid(g1, g2, ncol = 1), width = 10, height = 12)
