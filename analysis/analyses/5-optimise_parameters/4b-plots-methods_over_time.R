library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")


list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

methods <- read_rds(derived_file("methods_evaluated.rds", "4-method_characterisation")) %>%
  mutate(
    date_pub = as.POSIXct(PubDate),
    date_preprint = as.POSIXct(Preprint),
    date_earliest = as.POSIXct(ifelse(is.na(date_preprint), as.integer(date_pub), as.integer(date_preprint)), origin = "1970-01-01")
  )
methods <- map_df(seq_len(nrow(methods)), function(mi) {
  msel <- methods %>% slice(mi)
  method_ids <- str_split(msel$method_ids, ", ?")[[1]]
  msel %>% crossing(method_id = method_ids)
})

overall_score <- outputs_summtrajtype_totalsx2 %>%
  filter(task_source == "mean", trajectory_type == "overall")

comb <- overall_score %>%
  left_join(methods, by = c("method_short_name" = "method_id")) %>%
  filter(!is.na(date_earliest)) %>%
  arrange(date_earliest) %>%
  mutate(leading = cummax(harm_mean) == harm_mean)

leading <- comb %>%
  filter(leading) %>%
  add_row(date_earliest = Sys.time(), harm_mean = max(comb$harm_mean))

g <- ggplot(comb, aes(date_earliest, harm_mean)) +
  geom_step(data = leading, colour = "red") +
  geom_point() +
  geom_text(aes(label = method_short_name), nudge_y = .01) +
  labs(x = "Time", y = "Overall score")

ggsave(figure_file("leading_method.pdf"), g, width = 6, height = 4)
