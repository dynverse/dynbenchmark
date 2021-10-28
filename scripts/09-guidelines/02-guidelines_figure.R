#' Generate the guidelines figure where we show the top 4 methods for every (set of) trajectory types, with some other information relevant to the end user

library(tidyverse)
library(dynbenchmark)

experiment("09-guidelines")

# get the results and method info
results <- read_rds(result_file("results.rds", "08-summary"))
methods <- load_methods()

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
  columns <- paste0("benchmark_tt_", trajectory_types)
  benchmark <- results %>%
    filter(
      method_id %in% method_ids_detectable
      #method_id %in% method_ids_soft_prior
    ) %>%
    select(method_id, !!columns) %>%
    gather(metric_id, benchmark, -method_id) %>%
    group_by(method_id) %>%
    summarise(benchmark = mean(benchmark)) %>%
    mutate(benchmark = dynutils::scale_minmax(benchmark))

  # get qc & scalability
  other_results <- results %>%
    select(
      method_id,
      qc_app_user_friendly,
      scaling_pred_time_cells1k_features100k,
      scaling_pred_time_cells10k_features10k,
      scaling_pred_time_cells100k_features1k,
      method_required_priors
    )

  # combine
  results_top <- left_join(
    benchmark,
    other_results,
    "method_id"
  )

  # get top methods
  method_ids_top <- benchmark %>% arrange(-benchmark) %>% top_n(top_n, benchmark) %>% pull(method_id)
  results_top %>%
    filter(method_id %in% method_ids_top) %>%
    gather(metric, value, -method_id) %>%
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
palette <- c("#d73027", "#fee08b","#9bcde1")
split_renderer <- function(breaks, labels, palette_name) {
  breaks <- c(-Inf, breaks)
  palette <- palette[floor((seq_along(breaks)) * length(palette)/(length(breaks)))]
  function(x) {
    index <- last(which(x >= breaks))

    lst(
      fill = palette[index],
      label = labels[index]
    )
  }
}
time_renderer <- function(breaks) {
  breaks <- c(breaks, Inf)
  palette <- palette[floor((seq_along(breaks)) * length(palette)/(length(breaks)))] %>% rev()

  function(time) {
    time <- as.numeric(time)
    timestr = case_when(
      time < 1 ~ "<1s",
      time < 60 ~ paste0(floor(time), "s"),
      time < 3600 ~ paste0(floor(time / 60), "m"),
      time < 3600 * 24 ~ paste0(floor(time / 3600), "h"),
      time < 3600 * 24 * 7 ~ paste0(floor(time / 3600 / 24), "d"),
      TRUE ~ ">7d"
    )

    index <- first(which(time <= breaks))

    list(
      fill = palette[index],
      label = timestr
    )
  }
}

prior_renderer <- function(x) {
  prior2name <- dynwrap::priors %>% select(prior_id, name) %>% deframe()

  if (all(c("start_id", "end_id") %in% x)) {
    x <- x[!x %in% c("start_id", "end_id")]
    x <- c(x, "both_id")
    prior2name["both_id"] <- "Start & end cells"
  }

  if (all(c("start_n", "end_n") %in% x)) {
    x <- x[!x %in% c("start_n", "end_n")]
    x <- c(x, "both_n")
    prior2name["both_n"] <- "Number of end & start states"
  }

  list(
    fill = ifelse(length(x) == 0, "white", palette[[1]]),
    label = ifelse(length(x) == 0, "", glue::glue_collapse(prior2name[x], ","))
  )
}

category_x_padding <- 0.1
triple_checks <- c("-", "\U00B1", "+")
metrics <- tribble(
  ~id, ~name, ~renderer, ~category, ~width,
  "benchmark", "\n\nAccuracy", split_renderer(c(0.6, 0.95), triple_checks), "benchmark",1,
  "qc_app_user_friendly", "\n\nUsability", split_renderer(c(0.6, 0.9), triple_checks), "qc",1,
  "scaling_pred_time_cells100k_features1k", paste0("\n\n100k\U00D7", "1k"), time_renderer(c(60, 60*60)),  "scalability",1,
  "scaling_pred_time_cells10k_features10k", paste0("Estimated running time\n(cells \U00D7 features)\n10k\U00D7", "10k"), time_renderer(c(60, 60*60)), "scalability",1,
  "scaling_pred_time_cells1k_features100k", paste0("\n\n1k\U00D7", "100k"), time_renderer(c(60, 60*60)), "scalability",1,
  "method_required_priors", "\n\nRequired priors", prior_renderer, "priors", 4
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

setdiff(results_outcomes$metric, names(metric2renderer))

results_outcomes <- results_outcomes %>%
  mutate(
    render = pmap(lst(renderer = metric2renderer[metric], value), function(renderer, value) {renderer(value)})
  ) %>%
  mutate(
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
ggsave(result_file("guidelines_methods.svg"), width = 8, height = 10)

# the "tree.svg" file has a link to "guidelines_methods.svg", so make sure both are in the same directory
file.copy(raw_file("tree.svg"), result_file("guidelines.svg"), overwrite = TRUE)

# embed the methods svg (not supported, we have to choose between saving guidelines_methods as png and embedding, or as svg and linking)
system(glue::glue("inkscape {result_file('guidelines.svg')} --export-filename={result_file('guidelines.pdf')} --export-type=pdf"))
file.remove(result_file("guidelines.svg"))
