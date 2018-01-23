library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)


experiment("dataset_characterisation")

tasks <- readRDS(derived_file("tasks.rds"))

## Pie charts --------------------------------
tasks_real <- tasks %>% filter(category == "real")

technology_colors <- unique(tasks_real$technology) %>% {setNames(RColorBrewer::brewer.pal(length(.), "Set1"), .)}

pie <- function(tasks, what = "technology") {
  if (what == "standard") {
    fill_scale <- scale_fill_manual(values=c("gold"="#ffeca9", "silver"="#e3e3e3"))
  } else if (what == "technology") {
    fill_scale <- scale_fill_manual(values=technology_colors)
  } else {
    fill_scale <-scale_fill_grey(start = 0.6)
  }

  tasks$variable_of_interest <- tasks[[what]]
  tasks %>%
    count(variable_of_interest) %>%
    arrange(n) %>%
    arrange(rbind(seq_len(n()),rev(seq_len(n())))[seq_len(n())]) %>% # do largest - smallest - second largest - second smallest - ...
    mutate(odd = row_number()%%2, row_number = row_number()) %>%
    mutate(variable_of_interest = factor(variable_of_interest, levels=variable_of_interest)) %>%
    mutate(start = lag(cumsum(n), 1, 0), end = cumsum(n), mid = start + (end - start)/2) %>%
    ggplot(aes(ymin=0, ymax=1)) +
    geom_rect(aes(xmin=start, xmax=end, fill=variable_of_interest), stat="identity", color="white", size=0.5) +
    geom_text(aes(mid, odd, label = pritt("{label_short(variable_of_interest)}"), vjust=1-odd), color="black", hjust=0.5, size=5) +
    geom_text(aes(mid, 0.5, label = pritt("{n}")), color="black", hjust=0.5, size = 5) +
    fill_scale +
    ggtitle(label_long(what)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_x_continuous(expand=c(0.08, 0.08)) +
    scale_y_continuous(limits=c(-0.5, 1.5))
}

dataset_characterisation_pies <- c("technology", "organism", "standard", "trajectory_type") %>% map(pie, tasks = tasks_real) %>% cowplot::plot_grid(plotlist=., ncol =1)
dataset_characterisation_pies


## Distributions -------------------------------------
dataset_characterisation_distributions <- c("ncells", "ngenes") %>% map(function(what) {
  tasks_real %>% mutate(what = map_int(normalisation_info, ~last(.$normalisation_steps[[what]]))) %>%
    ggplot(aes(what, fill=technology, color=technology)) +
    annotation_logticks(sides = "b", color = "#333333") +
    geom_dotplot(binwidth=0.025, method = "histodot", binpositions="all", stackgroups=TRUE, stackratio=1.5, dotsize=0.7) +
    scale_x_log10(paste0(label_long(what), " after filtering"), breaks=c(10, 20, 50, 100, 200, 500, 100, 1000, 2000, 5000, 10000), limits=c(50, 10000)) +
    scale_y_continuous(NULL, breaks=NULL, expand=c(0.02, 0)) +
    scale_fill_manual(values=technology_colors) +
    scale_color_manual(values=technology_colors) +
    theme(legend.position="none")
}) %>% cowplot::plot_grid(plotlist=., ncol=1)
dataset_characterisation_distributions


## Final figure -----------------------
dataset_characterisation_plot <- cowplot::plot_grid(
  dataset_characterisation_pies,
  dataset_characterisation_distributions,
  rel_heights = c(0.6, 0.35),
  ncol=1
)
dataset_characterisation_plot


cowplot::save_plot(figure_file("dataset_characterisation.svg"), dataset_characterisation_plot, base_height = 6, base_width = 6)
