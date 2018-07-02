library(dynutils)
library(tidyverse)
library(cowplot)
library(dynverse)

experiment("artificial/parameter_optimization")

samples <- expand.grid(
  a = seq(0, 20, length.out = 100),
  b = seq(0, 20, length.out = 100)#c(1,2,3,4,5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
) %>% as_tibble()

scores1 <- mvtnorm::dmvnorm(samples[, 1:2], c(8, 12), matrix(c(20, 3, 3, 8), nrow = 2))
scores2 <- mvtnorm::dmvnorm(samples[, 1:2], c(2, 4), matrix(c(2, -1, -1, 5), nrow = 2))

samples$score <- scores2 / max(scores2) * 0.4 + scores1 / max(scores1)

size <- c(3, 3)

base_plot <- samples %>% ggplot(aes(a, b)) +
  geom_contour(aes(z = score, color = ..level..), alpha = 0.4, bins = 8) +
  viridis:::scale_color_viridis() +
  theme(legend.position = "none")
ggsave(figure_file("base_plot.svg"), base_plot, width = size[[1]], height = size[[2]])

initial_samples <- samples %>% sample_n(100)
initial_samples_plot <- base_plot + geom_point(aes(color = score), initial_samples, shape = 4)
initial_samples_plot
ggsave(figure_file("initial_samples_plot.svg"), initial_samples_plot, width = size[[1]], height = size[[2]])

next_samples <- samples %>% sample_n(25, weight = score ** 3)
next_samples_plot <- base_plot +
  geom_point(aes(color = score), initial_samples, alpha = 0.5, shape = 4) +
  geom_point(aes(color = score), next_samples, size = 2.5)
next_samples_plot
ggsave(figure_file("next_samples_plot.svg"), next_samples_plot, width = size[[1]], height = size[[2]])
