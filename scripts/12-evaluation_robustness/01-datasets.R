library(tidyverse)
library(dynbenchmark)

library(furrr)
plan(multiprocess)


experiment("12-evaluation_robustness")

list2env(read_rds(result_file("benchmark_results_input.rds", "07-benchmark")), environment())
raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", "07-benchmark"))$raw_data

aggregate <- function(raw_data) {
  out <- benchmark_aggregate(
    data = raw_data,
    metrics = metrics,
    norm_fun = norm_fun,
    mean_fun = mean_fun,
    mean_weights = mean_weights,
    dataset_source_weights = dataset_source_weights
  )
}

get_overall_score <- function(raw_data) {
  out <- aggregate(raw_data)

  out$data_aggregations %>%
    filter(dataset_trajectory_type == "overall", dataset_source == "mean") %>%
    select(method_id, overall)
}

rank_methods <- function(raw_data) {
  get_overall_score(raw_data) %>% deframe() %>% rank(ties.method = c("max"))
}

##  ............................................................................
##  Dataset variability                                                     ####
out <- aggregate(raw_data)
overall_scores <- get_overall_score(raw_data)
method_order <- overall_scores %>% arrange(-overall) %>% pull(method_id)

plot_dataset_variability <- out$data %>%
  slice(sample(n())) %>%
  mutate(
    dataset_origin = gsub("([^/]*).*", "\\1", dataset_source)
  ) %>%
  ggplot(aes(factor(method_id, method_order), overall, color = dataset_origin)) +
  ggbeeswarm::geom_quasirandom() +
  geom_point(data = overall_scores %>% mutate(dataset_origin = "mean")) +
  scale_x_discrete("", labels = label_method) +
  scale_y_continuous("Overall score") +
  scale_color_manual(label_long("dataset_source"), values = c(mean = "black", synthetic = "#39CCCC", real = "#FF4136"), label = label_long) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top", legend.justification = "center")

plot_dataset_variability
write_rds(plot_dataset_variability, result_file("dataset_variability.rds"))


##  ............................................................................
##  Subsample datasets                                                      ####

# subsample datasets, calculate overall benchmark score
all_dataset_ids <- unique()

n_samples <- 10
dataset_sample_scores <- pbapply::pblapply(cl = 8, 1:length(all_dataset_ids), function(n_datasets) {
  map(1:n_samples, function(datasets_sample_i) {
    dataset_ids <- sample(all_dataset_ids, n_datasets)
    method_scores <- rank_methods(
      ind_scores %>% filter(dataset_id %in% dataset_ids)
    ) %>%
      mutate(n_datasets = n_datasets, datasets_sample_i = datasets_sample_i)
  }) %>% bind_rows()
}) %>% bind_rows()

# plot harm mean correlations across subsamples
dataset_sample_correlations <- dataset_sample_scores %>%
  group_by(n_datasets, datasets_sample_i) %>%
  slice(match(method_scores$method_short_name, method_short_name)) %>%
  do(cor = cor(.$overall_benchmark, method_scores$overall_benchmark, method = "spearman")) %>%
  ungroup() %>%
  mutate(cor = unlist(cor)) %>%
  mutate(n_datasets_perc = n_datasets / length(all_dataset_ids))
write_rds(dataset_sample_correlations, result_file("dataset_sample_correlations.rds"))

dataset_sample_correlations_boxplot <- dataset_sample_correlations %>%
  ggplot(aes(n_datasets, cor, group = n_datasets)) +
  geom_boxplot() +
  geom_hline(yintercept = 1) +
  scale_y_continuous(label_long("correlation_between_method_ordering"), expand = c(0, 0), limits = c(0,1)) +
  scale_x_continuous(label_long("n_datasets"), expand = c(0, 0), breaks = c(1, seq(10, length(all_dataset_ids)-10, 10), length(all_dataset_ids)))
dataset_sample_correlations_boxplot

# calculate rank variability of each method
dataset_sample_ranks <- dataset_sample_scores %>%
  group_by(n_datasets, datasets_sample_i) %>%
  mutate(harm_mean_rank = rank(-harm_mean)) %>%
  ungroup()

dataset_sample_ranks_match <- dataset_sample_ranks %>%
  group_by(method_short_name, n_datasets) %>%
  summarise(rank_match = mean(harm_mean_rank == match(method_short_name, method_order)))
write_rds(dataset_sample_ranks_match, result_file("dataset_sample_ranks_match.rds"))
out <- aggregate(raw_data)
dataset_sample_ranks_match %>%
  ggplot() +
  geom_line(aes(n_datasets, rank_match, color = method_short_name))

# calculate n times a method is in the top
dataset_sample_n_top <- dataset_sample_ranks %>%
  group_by(n_datasets, method_short_name) %>%
  summarise(n_top = sum(harm_mean_rank == 1)) %>%
  mutate(method_short_name = factor(method_short_name, method_order)) %>%
  ungroup()

dataset_sample_n_top %>%
  filter(n_datasets <= 10) %>%
  ggplot(aes(method_short_name, n_top)) +
  geom_bar(stat = "identity")

# calculate the maximal subset of datasets at which a method is top
# this is based on subsampling, see beneath for a smarter method
dataset_sample_max_datasets_top <- dataset_sample_n_top %>%
  filter(n_top > 1) %>%
  group_by(method_short_name) %>%
  summarise(max_n_datasets_top = max(n_datasets)) %>%
  complete(method_short_name, fill = list(max_n_datasets_top = 0))



##  ............................................................................
##  Subset of datasets for which method is top                              ####
# get the rank of each method within each dataset
data <- aggregate(raw_data)$data %>%
  group_by(dataset_id) %>%
  mutate(overall_rank = rank(-overall)) %>%
  ungroup()

all_dataset_ids <- unique(data$dataset_id)
all_method_ids <- unique(data$method_id)

# get the rank of a method given a set of include datasets
method_scorer <- function(method_id) {
  function(include_datasets) {
    if(sum(include_datasets) == 0) {
      length(all_method_ids)
    } else {
      overall_scores <- get_overall_score(
        data %>% filter(dataset_id %in% names(include_datasets)[include_datasets])
      )

      overall_scores %>%
        mutate(overall_rank = rank(-overall)) %>%
        filter(method_id == !!method_id) %>%
        pull(overall_rank)
    }
  }
}

get_toprank_dataset_subset <- function(method_id, raw_data) {
  devtools::load_all(paste0(dynbenchmark::get_dynbenchmark_folder(), "/package"))

  # first rank the methods within each dataset, then use this ranking to determine which datasets are optimal for each method
  # this determines the order of the datasets which will be removed
  dataset_removal_order <- data %>%
    group_by(dataset_id) %>%
    mutate(overall_rank = rank(-overall)) %>%
    ungroup() %>%
    filter(method_id == !!method_id) %>%
    arrange(-overall_rank) %>%
    pull(dataset_id)

  # define a the datasets to be included
  include_datasets <- rep(TRUE, length(dataset_removal_order)) %>% set_names(dataset_removal_order)

  # get a scorer for this method
  scorer <- method_scorer(method_id)

  # get the initial rank
  rank <- scorer(include_datasets)

  # remove datasets until the rank is 1
  if (rank != 1) {
    for(remove_dataset_id in dataset_removal_order) {
      include_datasets[remove_dataset_id] <- FALSE

      rank <- scorer(include_datasets)
      print(rank)

      if(rank == 1) {
        break
      }
    }
  }

  sum(include_datasets)
}

# n_dataset_for_top <- tibble(
#   method_id = c("scorpius", "gng", "pcreode"),
#   n_datasets = future_map_int(method_id, get_toprank_dataset_subset, raw_data = raw_data)
# )

n_dataset_for_top <- tibble(
  method_id = all_method_ids
)

results <- qsub_lapply(
  n_dataset_for_top$method_id[1:4],
  get_toprank_dataset_subset,
  qsub_config = override_qsub_config(memory = "2G"),
  qsub_packages = c("tidyverse"),
  raw_data = raw_data
)

n_dataset_for_top <- n_dataset_for_top %>% bind_rows()

write_rds(dataset_sample_max_datasets_top, result_file("dataset_sample_max_datasets_top.rds"))
dataset_sample_max_datasets_top <- read_rds(result_file("dataset_sample_max_datasets_top.rds"))

dataset_sample_max_datasets_overview <- dataset_sample_max_datasets_top %>%
  mutate(method_short_name = factor(method_short_name, method_order)) %>%
  ggplot(aes(method_short_name, max_n_datasets_top)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = max_n_datasets_top), vjust = 0) +
  geom_hline(yintercept = length(all_dataset_ids), linetype = "dashed") +
  geom_text(aes(y = length(all_dataset_ids), x = nrow(dataset_sample_max_datasets_top)), label = "All datasets", hjust = 1, vjust = 1.5, data = tibble()) +
  scale_x_discrete("", labels = method_names) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(label_short("largest_subset_of_datasets_at_which_method_ranks_first", 30), limits = c(0, length(all_dataset_ids)+10), expand = c(0, 0))
dataset_sample_max_datasets_overview %>% write_rds(result_file("dataset_sample_max_datasets_overview.rds"), compress = "xz")


dataset_sample_max_datasets_top_individual <- dataset_sample_max_datasets_top %>%
  unnest(dataset_ids) %>%
  rename(dataset_id = dataset_ids) %>%
  left_join(datasets, c("dataset_id" = "id"))

dataset_sample_max_datasets_top_individual %>%
  mutate(method_short_name = factor(method_short_name, method_order)) %>%
  ggplot(aes(method_short_name, fill = trajectory_type)) +
  geom_bar() +
  scale_fill_manual(values = set_names(trajectory_types$colour, trajectory_types$id))

##  ............................................................................
##  Dataset technology influence                                            ####
method_scores_real <- outputs_list$outputs_summmethod_totals %>%
  filter(dataset_source == "real") %>%
  rename(method_short_name = method_short_name)
method_scores_technologies <- map(unique(datasets$technology) %>% discard(is.na), function(technology_id) {
  method_ranking <- ind_scores %>%
    filter(dataset_id %in% datasets$dataset_id[datasets$technology == technology_id]) %>%
    rank_methods() %>%
    mutate(technology_id = technology_id)
}) %>% bind_rows()
method_scores_technologies <- method_scores_technologies %>%
  select(method_short_name, technology_id, harm_mean) %>%
  rename(harm_mean_technology = harm_mean) %>%
  left_join(method_scores_real %>% rename(harm_mean_overall = harm_mean), by = "method_short_name")

method_scores_technologies_cors <- method_scores_technologies %>%
  group_by(technology_id) %>%
  summarise(cor = cor(harm_mean_technology, harm_mean_overall))



method_scores_technologies %>%
  ggplot(aes(harm_mean_overall, harm_mean_technology)) +
  geom_point() +
  facet_wrap(~technology_id) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(aes(label = round(cor, 2)), x = 0.1, y = max(method_scores_technologies$harm_mean_overall), data = method_scores_technologies_cors, size = 5) +
  coord_equal() +
  theme_bw() +
  theme(
    legend.position = "none", panel.grid = element_blank()
  )
