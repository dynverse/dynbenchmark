#   ____________________________________________________________________________
#   Loading                                                                 ####

library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("4-method_characterisation")

implementations_evaluated <- read_rds(derived_file("implementations_evaluated.rds"))
implementations <- read_rds(derived_file("implementations.rds"))
implementation_qc <- read_rds(derived_file("implementation_qc.rds"))

implementation_id_to_name <- setNames(implementations$implementation_name, implementations$implementation_id)

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2018-09-01")


#   ____________________________________________________________________________
#   Figure generation                                                       ####

##  ............................................................................
##  Publication timeline of implementations                                         ####

implementation_publication_data <- implementations %>% filter(is_ti) %>%
  gather("publication_type", "publication_date", c(PubDate, Preprint)) %>%
  select(implementation_id, publication_type, publication_date) %>%
  drop_na() %>%
  mutate(counter = 1)

# add published preprints
implementation_publication_data <- implementation_publication_data %>%
  group_by(implementation_id) %>%
  filter(n() == 2) %>%
  filter(publication_type == "PubDate") %>%
  mutate(publication_type = "Preprint", counter = -1) %>%
  bind_rows(implementation_publication_data) %>%
  mutate(publication_type = factor(publication_type, levels=c("PubDate", "Preprint")))

# summarise by publication type how many implementations there are
publication_cumulative_by_type <- implementation_publication_data %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(n_implementations = cumsum(counter)) %>%
  ungroup()
# add first and last
earliest_date <- min(publication_cumulative_by_type$publication_date)
latest_date <- Sys.Date() + 100
earliest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  filter(row_number() == 1) %>%
  mutate(n_implementations = 0, publication_date = earliest_date, implementation_id = NA, counter=0) %>%
  ungroup()
latest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  filter(row_number() == n()) %>%
  mutate(publication_date = latest_date, implementation_id = NA, counter = 0) %>%
  ungroup()
publication_cumulative_by_type <- bind_rows(earliest, publication_cumulative_by_type, latest)

ggplot(publication_cumulative_by_type) +
  geom_step(aes(publication_date, n_implementations, color=publication_type)) +
  scale_x_date(limits=c(start_date, end_date))

# the following code is very complex, don't try to understand it, I don't either
# for geom_area it is important that every date is represented in every group
# therefore we add rows here to add a row for publication when a preprint was published and vice versa
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type %>%
  arrange(publication_date) %>%
  complete(publication_type, publication_date) %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
  mutate(n_implementations = cumsum(counter)) %>%
  ungroup()

# now we calculate cumulative number of implementations
# although not necessary with geom_area and position="stack", this is necessary to add labels
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  group_by(publication_date) %>%
  arrange(-as.numeric(publication_type)) %>%
  mutate(n_implementations_cumulative = cumsum(n_implementations)) %>%
  ungroup()

# now we make the plot step-like, by adding an extra row just before every row with n_implementations equal to the previous row
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  arrange(publication_type, publication_date) %>%
  mutate(n_implementations_cumulative = lag(n_implementations_cumulative, 1), publication_date = publication_date - 0.1, counter=0, prefix=TRUE) %>%
  drop_na(n_implementations_cumulative) %>%
  bind_rows(publication_cumulative_by_type_interpolated) %>%
  arrange(publication_type, publication_date, n_implementations_cumulative)

# now we calculate text positions, merge labels when they are on the same row
publication_cumulative_by_type_interpolated_text <- publication_cumulative_by_type_interpolated %>%
  filter(is.na(prefix)) %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(run = rle(n_implementations_cumulative) %>% {rep(seq_along(.$lengths), .$lengths)}) %>%
  filter(counter == 1) %>%
  group_by(publication_type, n_implementations_cumulative, run) %>%
  summarise(implementation_id = paste0(implementation_id, collapse="   "), publication_date = min(publication_date))

publication_cumulative_text <- implementation_publication_data %>%
  group_by(implementation_id) %>%
  arrange(publication_date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(n_implementations_cumulative = seq_len(n()))


n_implementations_over_time <- publication_cumulative_by_type_interpolated %>%
  mutate(publication_type = c(PubDate="peer_reviewed", Preprint = "preprint")[publication_type]) %>%
  ggplot() +
    geom_area(aes(publication_date, n_implementations_cumulative, fill=publication_type), position="identity") +
    scale_fill_manual("", labels=label_long, values=c(peer_reviewed="#334466", preprint="#445588")) +
    geom_text(
      aes(
        publication_date + 1,
        n_implementations_cumulative - 0.5,
        label=implementation_id_to_name[implementation_id],
        group=publication_type
      ),
      data = publication_cumulative_text,
      color="white",
      vjust=0.5,
      hjust=0,
      fontface = "bold",
      size=3
    ) +
    theme(legend.position = c(0.05, 0.95)) +
  scale_y_continuous(label_long("n_implementations"), expand=c(0, 0)) +
  scale_x_date(label_long("publication_date"), expand=c(0.05, 0.05), limits=c(start_date, end_date))
n_implementations_over_time
# ggsave(figure_file("n_implementations_over_time.png"), n_implementations_over_time, width = 15, height = 8)
saveRDS(n_implementations_over_time %>% ggdraw(), figure_file("n_implementations_over_time.rds"))


##  ............................................................................
##  Platforms                                                               ####
platforms <- implementations %>% separate_rows(platform=platforms, sep=", ") %>%
  group_by(platforms) %>%
  summarise(quantity = n()) %>%
  arrange(quantity) %>%
  mutate(platform = ifelse(is.na(platforms), "Not available", platforms)) %>%
  mutate(platform = factor(platform, levels=rev(platform))) %>%
  ungroup() %>%
  mutate(pos = cumsum(quantity) - quantity/2) %>%
  ggplot(aes(1, quantity)) +
    geom_bar(aes(fill=platform), width = 1, stat="identity") +
    geom_text(aes(1, pos, label=pritt("{platform} \n {quantity}"), fill=platform), color="white", fontface = "bold", direction = "y", segment.alpha=0) +
    theme_void() +
    theme(legend.position="none") +
    coord_flip() +
    coord_polar("y")
platforms
saveRDS(platforms, figure_file("platforms.rds"))



##  ............................................................................
##  Trajectory types over time                                              ####
directed_trajectory_type_order <- trajectory_types %>% filter(directedness == "directed") %>% pull(id) %>% keep(~.!="unknown")

trajectory_components <- implementations %>% filter(is_ti)
trajectory_components <- trajectory_components %>%
  arrange(date) %>%
  mutate(count = 1)

add_step <- function(df) {
  df <- df %>%
    mutate(date = date - 0.2, prefix=TRUE, count = 0) %>%
    bind_rows(df)
  df %>% arrange(date)
}

trajectory_components_step <- add_step(arrange(trajectory_components, date))

trajectory_components_gathered <- trajectory_components_step %>%
  gather(trajectory_type, can_trajectory_type, !!directed_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=directed_trajectory_type_order)) %>%
  group_by(trajectory_type) %>%
  arrange(date) %>%
  mutate(n_implementations = cumsum(count), n_implementations_oi = cumsum(count * can_trajectory_type))

trajectory_components_over_time <- trajectory_components_gathered %>%
  arrange(date) %>%
  ggplot() +
  geom_area(aes(date, n_implementations), stat="identity", fill="#BBBBBB") +
  geom_area(aes(date, n_implementations_oi, fill=trajectory_type), stat="identity") +
  scale_fill_manual(values=setNames(trajectory_types$color, trajectory_types$id)) +
  facet_grid(.~trajectory_type, labeller = label_facet()) +
  theme(legend.position = "none") +
  scale_x_date(label_long("publication_date"), limits=c(start_date, end_date), breaks=c(start_date, end_date), labels=c("", ""), expand=c(0, 0)) +
  scale_y_continuous(label_long("n_implementations"), expand=c(0, 0))

trajectory_components_over_time

saveRDS(trajectory_components_over_time, figure_file("trajectory_components_over_time.rds"))



##  ............................................................................
##  Create timeline overview figure                                         ####
n_methods_over_time <- read_rds(figure_file("n_implementations_over_time.rds", experiment_id="4-method_characterisation"))
trajectory_components_over_time <- read_rds(figure_file("trajectory_components_over_time.rds", experiment_id="4-method_characterisation"))

methods_timeline <- cowplot::plot_grid(plotlist=list(n_methods_over_time, trajectory_components_over_time), nrow=2, rel_heights = c(0.7, 0.3), labels="auto")
methods_timeline %>% save_plot(figure_file("methods_timeline.svg"), ., base_width=15, base_height=10)
methods_timeline %>% save_plot(figure_file("methods_timeline.png"), ., base_width=15, base_height=10)


##  ............................................................................
##  implementations table                                                           ####
implementations$non_inclusion_reasons_footnotes <- implementations$non_inclusion_reasons_split %>%
  map(function(reasons) {
    slice(non_inclusion_reasons, match(reasons, id))$footnote
  })

superscript <- c(latex = function(x) pritt("\\textsuperscript{{{x}}}"), html=function(x) pritt("<sup>{x}</sup>"))

table <- map(c("latex", "html"), function(format) {
  implementations_table <- implementations %>%
    filter(is_ti) %>%
    arrange(date) %>%
    mutate(
      evaluated = ifelse(evaluated, "Yes", map_chr(non_inclusion_reasons_footnotes, ~paste0("No" , superscript[[format]](paste0(., collapse=" "))))),
      evaluated = kableExtra::cell_spec(
        evaluated,
        format,
        color=ifelse(evaluated == "Yes", "green", "black"),
        escape = F
      ),
      date = strftime(date, "%d/%m/%Y"),
      maximal_trajectory_type =
        kableExtra::cell_spec(
          label_long(maximal_trajectory_type),
          format,
          background=toupper(set_names(trajectory_types$color, trajectory_types$id)[maximal_trajectory_type]),
          color=ifelse(maximal_trajectory_type == "rooted_tree", "#333333", "#FFFFFF"),
        ),
      implementation_name = kableExtra::cell_spec(
        implementation_name,
        format,
        link=Code)
    ) %>%
    select(implementation_name, date, maximal_trajectory_type, evaluated) %>%
    rename_all(label_long)

  table <- implementations_table %>%
    knitr::kable(format, escape=F) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover","condensed"), font_size=ifelse(format == "latex", 7, 12)) %>%
    kableExtra::add_footnote(non_inclusion_reasons$long, "number")
  table
}) %>% set_names(c("latex", "html"))
table %>%
  saveRDS(figure_file("implementations_table.rds"))



##  ............................................................................
##  Small implementation timeline                                                   ####
implementation_small_history_data <- implementations %>%
  filter(is_ti) %>%
  mutate(
    maximal_trajectory_type = ifelse(is.na(maximal_trajectory_type), "unknown", as.character(maximal_trajectory_type)),
    y = runif(n(), 0, 0.0001),
    fontface = ifelse(evaluated, "plain", "plain"),
    size = ifelse(evaluated, 3.5, 3.5)
  ) %>%
  mutate() %>%
  arrange(-y)
implementation_small_history <- implementation_small_history_data %>%
  ggplot(aes(date, y)) +
  ggrepel::geom_label_repel(
    aes(
      fill=maximal_trajectory_type,
      color=maximal_trajectory_type,
      label=implementation_id_to_name[implementation_id],
      fontface=fontface,
      size=size
    ),
    direction="y", max.iter=10000, ylim=c(0, NA), force=10, min.segment.length = 0) +
  # ggrepel::geom_label_repel(aes(fill=maximal_trajectory_type, label=implementation_id, fontface=fontface, size=size), direction="y", max.iter=10000, ylim=c(0, NA), force=10, min.segment.length = 999999) +
  scale_color_manual(values=setNames(trajectory_types$color, trajectory_types$id)) +
  scale_fill_manual(values=setNames(trajectory_types$background_color, trajectory_types$id)) +
  scale_alpha_manual(values=c(`TRUE`=1, `FALSE`=0.6)) +
  scale_x_date(label_long("publishing_date"), limits=c(as.Date("2014-01-01"), as.Date("2018-04-01")), date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(NULL, breaks=NULL, limits=c(0, 1), expand=c(0, 0)) +
  scale_size_identity() +
  theme(legend.position="none")
implementation_small_history

ggsave(figure_file("implementation_small_history.svg"), implementation_small_history, height = 4.5, width = 10)

# move lines to back
library(xml2)
svg <- xml2::read_xml(figure_file("implementation_small_history.svg"))

children <- svg %>% xml_children()
front <- xml_name(children) %in% c("line")
new <- children[!front]
for (node in children[front]) {
  new %>% xml_add_sibling(node, .copy = FALSE, where=c("before"))
}
children[xml_name(children) == "rect"] %>% xml_remove()

# text color
texts <- xml_children(svg) %>% {xml_children(.)} %>% {.[xml_name(.) == "text"]} %>% {.[str_detect(xml_attr(., "style"), "fill:")]}
xml_attr(texts, "style") <- xml_attr(texts, "style") %>% gsub("fill: #[A-Z0-9]*;", "fill: #000;", .)

xml2::write_xml(svg, figure_file("implementation_small_history.svg"))



##  ............................................................................
##  Small trajectory type distribution                                      ####

implementation_small_distribution <- implementations %>%
  filter(is_ti) %>%
  gather(trajectory_type, can_handle, !!directed_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=directed_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count() %>%
  ggplot(aes(trajectory_type, n)) +
  geom_bar(aes(fill=trajectory_type, color=trajectory_type), stat="identity", width=0.95) +
  geom_text(aes(label=n), vjust=0) +
  # geom_hline(yintercept = sum(implementations$is_ti, na.rm=TRUE), line) +
  scale_fill_manual(values=setNames(trajectory_types$background_color, trajectory_types$id)) +
  scale_color_manual(values=setNames(trajectory_types$color, trajectory_types$id)) +
  scale_y_continuous(expand=c(0, 2)) +
  theme(legend.position = "None")
implementation_small_distribution
ggsave(figure_file("implementation_small_distribution.svg"), implementation_small_distribution, height = 2, width = 5)



implementations %>%
  filter(is_ti) %>%
  gather(trajectory_type, can_handle, !!directed_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=directed_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count()

