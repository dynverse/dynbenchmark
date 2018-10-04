#' Comparison between predicted versus actual running time and memory usage

join <-
  data %>%
  filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
  mutate(
    time = ifelse(error_status == "memory_limit", NA, time),
    mem = ifelse(error_status == "time_limit" | mem < log10(100e6), NA, mem),
    ltime = log10(time),
    lmem = log10(mem)
  )

range_time <- quantile(abs(join$ltime - join$time_lpred), .9, na.rm = TRUE) %>% {2 * c(-., .)}
range_mem <- quantile(abs(join$lmem - join$mem_lpred), .9, na.rm = TRUE) %>% {2 * c(-., .)}

g1 <-
  ggplot(join) +
  geom_point(aes(time_lpred, ltime, colour = ltime - time_lpred)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = range_time)

g2 <-
  ggplot(join) +
  geom_point(aes(mem_lpred, lmem, colour = lmem - mem_lpred)) +
  theme_bw() +
  facet_wrap(~method_id, scales = "free") +
  scale_color_distiller(palette = "RdYlBu", limits = range_mem)

pdf(result_file("compare_pred_ind.pdf"), width = 20, height = 15)
print(g1 + labs(title = "Timings"))
print(g2 + labs(title = "Memory"))
dev.off()


g1 <- ggplot(join) +
  geom_point(aes(time_lpred, ltime, colour = ltime - time_lpred)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = range_time) +
  theme(legend.position = "bottom")
g2 <- ggplot(join %>% filter(lmem > 8)) +
  geom_point(aes(mem_lpred, lmem, colour = lmem - mem_lpred)) +
  theme_bw() +
  scale_color_distiller(palette = "RdYlBu", limits = range_mem) +
  theme(legend.position = "bottom")
ggsave(result_file("compare_pred_all.pdf"), patchwork::wrap_plots(g1, g2, nrow = 1), width = 16, height = 8)

rm(join, g1, g2)
