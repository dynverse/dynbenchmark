dat <- data_var
mids <- dat %>% filter(metric == "var_overall") %>% arrange(value) %>% pull(method_id)
dat$method_id <- factor(dat$method_id, levels = mids)

g <- ggplot(dat) +
  geom_point(aes(value, method_id)) +
  facet_wrap(~ metric, nrow = 1) +
  theme_bw() +
  labs(x = NULL, y = NULL)

ggsave(result_file("variances.pdf"), g, width = 20, height = 7)

rm(g, dat, mids)
