library(tidyverse)
library(dynbenchmark)
library(xml2)

experiment("09-guidelines")

# get the results and method info
results <- read_rds(result_file("results.rds", "08-summary"))$results
methods <- read_rds(result_file("methods.rds", "03-methods"))


extract_top_methods <- function(trajectory_types, top_n = 4) {
  # filter on non-hard priors
  method_ids_soft_prior <- methods %>%
    filter(
      !map_lgl(
        required_priors,
        ~ any(. %in% (dynwrap::priors %>% filter(type == "hard") %>% pull(prior_id)))
      )
    ) %>%
    pull(id)

  # filter on detectable
  detects_filter <- glue::glue_collapse(paste0("detects_", trajectory_types), " & ")
  method_ids_detectable <- methods %>%
    filter(!!rlang::parse_expr(detects_filter)) %>%
    pull(id)

  # get benchmark scores (average)
  benchmark <- results %>%
    filter(
      method_id %in% method_ids_detectable,
      method_id %in% method_ids_soft_prior,
      experiment == "benchmark",
      metric %in% trajectory_types
    ) %>%
    group_by(method_id) %>%
    summarise(value = mean(value)) %>%
    mutate(metric = "benchmark", experiment = "benchmark") %>%
    mutate(value = dynutils::scale_minmax(value))

  # get qc scores
  qc <- results %>%
    filter(experiment == "qc", metric %in% c("user_friendly")) %>%
    mutate(metric = "qc") %>%
    select(experiment, method_id, metric, label, value)

  # get scalability scores
  scalability <- results %>%
    filter(
      experiment == "scalability",
      metric %in% c("1k cells", "10k cells", "100k cells")
    ) %>%
    select(experiment, method_id, metric, label, value)

  # required priors
  required_priors <- methods %>%
    mutate(
      value = as.numeric(requires_prior),
      experiment = "priors",
      metric = "priors",
      label = map_chr(required_priors, function(required_priors) {
        dynwrap::priors$name[match(required_priors, dynwrap::priors$prior_id)] %>% paste0(collapse =  ", ")
      })
    ) %>%
    select(experiment, metric, method_id = id, value, label)

  # combine
  results_top <- bind_rows(
    benchmark,
    qc,
    scalability,
    required_priors
  )

  # get top methods
  method_ids_top <- benchmark %>% arrange(-value) %>% top_n(top_n, value) %>% pull(method_id)
  results_top %>%
    filter(method_id %in% method_ids_top) %>%
    mutate(method_rank = match(method_id, method_ids_top))
}

# outcomes
common_padding <- 0.6
outcomes <- tribble(
  ~id, ~trajectory_types, ~padding,
  "multiple_disconnected*", c("linear", "bifurcation", "multifurcation", "tree", "graph", "disconnected_graph"), common_padding,
  "graph", c("linear", "bifurcation", "multifurcation", "tree", "graph"), common_padding,
  "tree*", c("linear", "bifurcation", "multifurcation", "tree"), common_padding,
  "tree", "tree", common_padding,
  "multifurcation", "multifurcation", common_padding,
  "bifurcation", "bifurcation", common_padding,
  "linear", "linear", common_padding,
  "cycle", "cycle", common_padding
) %>%
  mutate(y = row_number()*4 + cumsum(padding))

# columns
palettes <-
  c(
    map(c("viridis", "magma", "cividis"), ~viridisLite::viridis(100, option = .)) %>% set_names(c("viridis", "magma", "cividis")),
    map(c("RdYlGn", "RdYlBu"), RColorBrewer::brewer.pal, n = 8) %>% set_names(c("RdYlGn", "RdYlBu")),
    list(
      wouter = c("#d73027", "#fdae61", "#ffd217", "#4575b4", "#313695"),
      wouter2 = c("#d73027", "#fee08b","#9bcde1")
    )
  )

# palette_names <- c(qc = "viridis", benchmark = "magma", scalability = "cividis")
# palette_names <- c(qc = "RdYlGn", benchmark = "RdYlGn", scalability = "RdYlGn")
# palette_names <- c(qc = "RdYlBu", benchmark = "RdYlBu", scalability = "RdYlBu")
# palette_names <- c(qc = "wouter", benchmark = "wouter", scalability = "wouter")
palette_names <- c(qc = "wouter2", benchmark = "wouter2", scalability = "wouter2")

split_renderer <- function(breaks, labels, palette_name) {
  breaks <- c(-Inf, breaks)
  palette <- palettes[[palette_name]]
  palette <- palette[floor((seq_along(breaks)) * length(palette)/(length(breaks)))]
  function(x, label) {
    index <- last(which(x >= breaks))

    lst(
      fill = palette[index],
      label = if(is.na(label)) {labels[index]} else {label}
    )
  }
}
time_renderer <- function(x, label) {
  list(
    fill = "white",
    label = label
  )
}
prior_renderer <- function(x, label) {
  list(
    fill = ifelse(label != "", palettes$RdYlGn[[1]], "white"),
    label = label
  )
}

quintuple_checks <- c("-", "\U00B1", "+")
# quintuple_checks <- c("\U2716", "\U2715", "\U2713", "\U2713\U2713", "\U2713\U2713\U2713")

category_x_padding <- 0.1
metrics <- tribble(
  ~id, ~name, ~renderer, ~category, ~width,
  "benchmark", "Benchmark\nscore", split_renderer(c(0.6, 0.95), quintuple_checks, palette_names["benchmark"]), "benchmark",1,
  "qc", "User\nFriendliness", split_renderer(c(0.6, 0.9), quintuple_checks, palette_names["qc"]), "qc",1,
  "1k cells", " \n1k cells", split_renderer(c(0.5, 0.8), quintuple_checks, palette_names["scalability"]),  "scalability",1,
  "10k cells", "Est. running time @ 10k features\n10k cells", split_renderer(c(0.5, 0.8), quintuple_checks, palette_names["scalability"]), "scalability",1,
  "100k cells", "\n100k cells", split_renderer(c(0.5, 0.8), quintuple_checks, palette_names["scalability"]), "scalability",1,
  "priors", "\nRequired priors", prior_renderer, "priors", 2
) %>%
  mutate(
    xmin = lag(cumsum(width), default = 0) + c(0, cumsum(tail(category, -1) != head(category, -1))) * category_x_padding,
    xmax = xmin + width,
    xmid = (xmin+xmax)/2
  )
metric2renderer <- metrics %>% select(id, renderer) %>% deframe()

# calculate top methods per outcome
results_outcomes <- outcomes %>%
  rename_all(~paste0("outcome_", .)) %>%
  mutate(top_methods = map(outcome_trajectory_types, extract_top_methods)) %>%
  unnest(top_methods)

results_outcomes <- results_outcomes %>%
  mutate(
    render = pmap(lst(renderer = metric2renderer[metric], value, label), function(renderer, value, label) {renderer(value, label)}),
    fill = map_chr(render, "fill"),
    label = map_chr(render, "label"),
    color = ifelse(shades::lightness(fill) > 60, "black", "white")
  )

# calculate position of boxes and labels
results_boxes <- results_outcomes %>%
  left_join(metrics %>% select(metric = id, xmin, xmid, xmax), "metric") %>%
  mutate(
    ymin = outcome_y + method_rank,
    ymax = ymin + 1,
    ymid = (ymin+ymax)/2
  )

names_boxes <- results_boxes %>% group_by(outcome_id, method_id) %>% slice(1) %>% ungroup()

label_method <- function(method_ids) {methods$name[match(method_ids, methods$id)]}
results_boxes %>%
  ggplot(aes(x = xmid, y = ymid, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  # geom_text(aes(label = label, color = fill), size = 6, hjust = 0.5) +
  geom_rect(aes(fill = fill)) +
  geom_text(aes(label = label, color = color), size = 5, hjust = 0.5) +
  geom_text(aes(x = -0.1, label = label_method(method_id)), hjust = 1, data = names_boxes, size = 5) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(breaks = metrics$xmid, labels = metrics$name, position = "top", expand = c(0, 0), limits = c(-2.5, max(metrics$xmax))) +
  scale_y_reverse(expand = c(0, 0)) +
  theme_void() +
  theme(axis.text.x = element_text(vjust = 1, lineheight = 1))


# write the methods as svg and add to existing svg
ggsave(result_file("guidelines_methods.png"), width = 8, height = 10)

# the "tree.svg" file has a link to "guidelines_methods.svg", so make sure both are in the same directory
file.copy(raw_file("tree.svg"), result_file("guidelines.svg"), overwrite = TRUE)

# embed the methods svg (not supported, we have to choose between saving guidelines_methods as png and embedding, or as svg and linking)
system(glue::glue("inkscape {result_file('guidelines.svg')}  --verb=org.ekips.filter.embedimage.noprefs --verb=FileSave --verb=FileClose --verb=FileQuit"))

