library(tidyverse)
library(dynbenchmark)

library(ParamHelpers)
library(splatter)

library(qsub)

n_steps_per_length <- 100 # path.length (Splatter) for each milestone network edge length unit

# generate design of models, platforms and (randomised) splatter parameters
design_model <- crossing(
  model = c("linear", "bifurcating", "multifurcating", "binary_tree", "tree"),
  tibble(platform = load_platforms()) %>% mutate(platform_ix = row_number())
)

design_splatter_params <- ParamHelpers::generateDesign(
  nrow(design_model),
  ParamHelpers::makeParamSet(
    ParamHelpers::makeNumericParam("path.skew", lower = 0, upper = 1),
    ParamHelpers::makeNumericParam("path.nonlinearProb", lower = 0, upper = 1),
    ParamHelpers::makeNumericParam("path.sigmaFac", lower = 0, upper = 1),
    ParamHelpers::makeNumericParam("bcv.common.factor", lower = 10, upper = 200)
  )
)

design <- bind_cols(
  design_model,
  design_splatter_params
) %>% mutate(
  dataset_id = paste0("synthetic/splatter/", model, "_", platform_ix)
)

design_row <- extract_row_to_list(design, 25)

qsub_config <- override_qsub_config(name = "splatter", memory = "10G", wait = FALSE)

handle <- qsub_lapply(
  mapdf(design, identity),
  qsub_environment = NULL,
  qsub_config = qsub_config,
  qsub_packages = names(sessionInfo()$otherPkgs),
  function(design_row) {
  # load in the splatter simulation parameters
  splatter_params <- design_row$platform$estimate
  class(splatter_params) <- "SplatParams"

  milestone_network <- dyntoy::generate_milestone_network(design_row$model)

  # extract path.from, root is 0
  root <- setdiff(milestone_network$from, milestone_network$to)
  path.to <- c(root, milestone_network$to)
  path.from <- as.numeric(factor(milestone_network$from, levels = path.to)) - 1

  # factor added to bcv.common, influences how strong the biological effect is
  splatter_params@bcv.common <- splatter_params@bcv.common / design_row$bcv.common.factor

  # simulate
  sim <- splatSimulatePaths(
    splatter_params,
    batchCells = design_row$platform$n_cells,
    nGenes = design_row$platform$n_features,
    group.prob = milestone_network$length/sum(milestone_network$length),
    path.from = path.from,
    path.length = ceiling(milestone_network$length*n_steps_per_length),
    path.nonlinearProb = design_row$path.nonlinearProb,
    path.sigmaFac = design_row$path.sigmaFac,
    path.skew = design_row$path.skew
  )
  counts <- t(counts(sim))
  # expression <- t(exprs(scater::normalise(sim)))

  # normalise
  normalised <- dynnormaliser::normalise_filter_counts(counts, verbose = TRUE)
  counts <- normalised$counts
  expression <- normalised$expression

  # gold standard trajectory
  progressions <- milestone_network %>%
    dplyr::slice(as.numeric(gsub("Path([0-9]*)", "\\1", sim$Group))) %>%
    mutate(step = sim$Step, cell_id = as.character(sim$Cell), group = sim$Group) %>%
    group_by(from, to) %>%
    mutate(percentage = pmin(1, (step - 1) / ceiling(length * n_steps_per_length))) %>%
    ungroup() %>%
    select(cell_id, from, to, percentage)
  progressions <- progressions %>% filter(cell_id %in% rownames(counts))

  # wrap dataset
  dataset <- wrap_data(
    id = design_row$dataset_id,
    cell_ids = rownames(expression),
    dataset_source = "synthetic/splatter"
  ) %>%
    add_expression(
      counts = counts,
      expression = expression
    ) %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )

  # save dataset
  save_dataset(dataset, design_row$dataset_id)

  TRUE
})

qsub_retrieve(handle)



dynplot::plot_dimred(dataset, dimred=dyndimred::dimred_landmark_mds)
dynplot::plot_heatmap(dataset)
