#' Using the scores on perturbed datasets, assess whether the metrics follow certain rules

library(tidyverse)
library(dynbenchmark)

experiment("02-metrics/02-metric_conformity")

perturbation_methods <- dynbenchmark:::perturbation_methods_design

dataset_design <- read_rds(result_file("dataset_design.rds"))
perturbation_methods <- dynbenchmark:::perturbation_methods_design

# load rules
source(scripts_file("helper-rules.R"))

# load scores and models
scores <- read_rds(derived_file("scores.rds"))

# add harmonic mean
metric_ids_geom_mean <- metrics_evaluated %>% filter(category != "average") %>% pull(metric_id)
scores <- bind_rows(
  scores,
  scores %>%
    filter(metric_id %in% metric_ids_geom_mean) %>%
    group_by(method_id, dataset_id, param_id) %>%
    summarise(
      score = dyneval:::calculate_geometric_mean(score),
      metric_id = "geom_mean"
    )
)

# fix order of metrics
scores$metric_id <- factor(scores$metric_id, levels = unique(scores$metric_id))

# functions to assess conformity
filter_based_on_crossing <- function(x, crossing) {
  if (!all(colnames(crossing) %in% colnames(x))) {
    stop("All colnames in crossing should also be present in x")
  }

  y <- x %>% inner_join(crossing, colnames(crossing))

  if (nrow(y) < nrow(crossing)) {
    warning("Missing some results for: ", crossing %>% anti_join(x, colnames(crossing)), call. = FALSE)
  }

  y
}

assess_conformity <- function(rule, scores) {
  # extract only the relevant parts
  scores <- filter_based_on_crossing(scores, rule$crossing)

  print(rule$id)

  if (nrow(scores) == 0) {
    warning(rule$id)
  } else {
    assessment <- rule$assessment(scores)
    assessment$plot_datasets <- rule$plot_datasets()
  }
  assessment$rule_id <- rule$id

  assessment
}

assessments <-
  mapdf(rules, assess_conformity, scores = scores) %>%
  list_as_tibble() %>%
  mutate(rule_id = rules$id)

assessments %>%
  unnest(conformity) %>%
  ggplot(aes(metric_id, rule_id)) +
  geom_tile(aes(fill = conforms)) +
  scale_fill_manual(values = c(`TRUE` = "#2ECC40", `FALSE` = "#FF4136", `NA` = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# test one rule
rule <- rules %>% extract_row_to_list(which(rules$id == "cell_gathering"))
assessment <- assess_conformity(rule, scores)
assessment$plot_scores
assessment$plot_datasets

# save assessment
write_rds(assessments, result_file("assessments.rds"), compress = "xz")

# add image location to rules and save
rules <- rules %>%
  mutate(
    image_location = map_chr(id, function(id) fs::path_rel(result_file(paste0("images/", id, ".png")))),
    image_found = file.exists(image_location)
  )

if (any(!rules$image_found)) {stop("Files not found: ", basename(rules$image_location[!rules$image_found]))}

write_rds(rules, result_file("rules.rds"))
