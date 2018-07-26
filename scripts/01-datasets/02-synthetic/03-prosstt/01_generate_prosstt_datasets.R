library(reticulate)
library(tidyverse)
library(dynbenchmark)

# pip3 install git+https://github.com/soedinglab/prosstt
# based on https://github.com/soedinglab/prosstt/blob/master/examples/generate_simN.py

tree <- import("prosstt.tree")
sim <- import("prosstt.simulation")
sut <- import("prosstt.sim_utils")

n_steps_per_length <- 100 # number of steps in the simulation for each milestone network edge length unit

# generate design of models, platforms and (randomised) splatter parameters
design <- crossing(
  model = c("linear", "bifurcating", "multifurcating", "binary_tree", "tree"),
  tibble(platform = load_platforms()) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    a = runif(n(), 0.01, 0.1),
    intra_branch_tol = runif(n(), 0, 0.9),
    inter_branch_tol = runif(n(), 0, 0.9),
    alpha = exp(rnorm(n(), log(0.2), log(1.5))),
    beta = exp(rnorm(n(), log(1), log(1.5))) + 1
  ) %>%
  mutate(
    dataset_id = paste0("synthetic/prosstt/", model, "_", platform_ix)
  )

design_row <- extract_row_to_list(design, 2)
design_row <- extract_row_to_list(design, which(design$dataset_id == "synthetic/prosstt/linear_12"))

# generate all datasets, this can take some ~ 15 min
mapdf(design, function(design_row) {
  print(design_row$dataset_id)

  # generate milestone network
  milestone_network <- dyntoy::generate_milestone_network(design_row$model)

  # special case for the A->B network
  if (nrow(milestone_network) == 1) {
    milestone_network <- dyntoy::generate_milestone_network("linear", num_milestones = 2)
  }

  # generate branch network from milestone network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  milestone_network$branch_id <- as.character(seq_len(nrow(milestone_network)))
  branches <- milestone_network %>% select(branch_id, length, directed)
  branch_network <- map_df(milestone_ids, function(milestone_id) {
    crossing(
      from = milestone_network %>% filter(to == milestone_id) %>% pull(branch_id),
      to = milestone_network %>% filter(from == milestone_id) %>% pull(branch_id)
    )
  }) %>% filter(!is.na(to) & !is.na(from)) %>% select(from, to)

  # root branch network
  root <- setdiff(branch_network$from, branch_network$to)
  testthat::expect_equal(length(root), 1, info = "Need one and only one root, otherwise later functions will give random errors")
  branch_order <- igraph::graph_from_data_frame(branch_network) %>%
    igraph::ego(nodes = root, 999) %>%
    first() %>%
    names()
  branch_network <- branch_network %>% arrange(factor(from, branch_order), factor(to, branch_order))

  # create prosstt objects for topology, num_branches and time
  topology <- branch_network %>% pmap(function(from, to) c(from, to))
  num_branches <- nrow(branches)
  time <- branches %>% select(branch_id, length) %>% mutate(length = as.integer(pmax(2, ceiling(length * n_steps_per_length)))) %>% deframe() %>% as.list()

  # construct tree
  n_features <- min(1500L, design_row$platform$n_features) # limit number of features, because of extreme memory issues when simulating more than 2000 features
  t <- tree$Tree(topology, G = n_features, num_branches = num_branches, time = time, root = root)

  # simulate expression across lineage
  lin <- sim$simulate_lineage(
    t,
    a = design_row$a,
    intra_branch_tol = 0,
    inter_branch_tol = 0
  )
  uMs <- lin[[1]]
  gene_scale = sut$simulate_base_gene_exp(t, uMs)

  Ms <- map(branches$branch_id, function(branch) exp(uMs[[branch]]) %*% diag(gene_scale)) %>% set_names(branches$branch_id)
  t$add_genes(Ms)

  # actual simulation
  simulation <- sim$sample_density(t, no_cells = design_row$platform$n_cells, alpha = design_row$alpha, beta = design_row$beta)

  # process simulation output
  counts <- simulation[[1]]
  pseudotime <- simulation[[2]]
  brns <- simulation[[3]]

  rownames(counts) <- paste0("C", seq_len(nrow(counts)))
  colnames(counts) <- paste0("G", seq_len(ncol(counts)))

  # rescale global pseudotime to branch percentage
  branch_times <- t$branch_times()
  branch_progressions <- tibble(
    cell_id = rownames(counts),
    branch_id = brns,
    pseudotime = pseudotime,
    percentage = map2_dbl(pseudotime, branch_id, function(pseudotime, branch_id) {
      start <- branch_times[[branch_id]][[1]]
      end <- branch_times[[branch_id]][[2]]
      (pseudotime - start)/(end-start)
    })
  ) %>% select(cell_id,branch_id, percentage)

  # normalise & filter
  counts[counts < 0] <- 0 # in some cases, prosstt produces verry low numbers (eg. -8e564)
  normalised <- dynnormaliser::normalise_filter_counts(counts, verbose = TRUE)
  counts <- normalised$counts
  expression <- normalised$expression

  branch_progressions <- branch_progressions %>% filter(cell_id %in% rownames(counts))

  # create dataset
  dataset <- wrap_data(
    id = design_row$dataset_id,
    rownames(counts),
    dataset_source = "synthetic/prosstt"
  ) %>%
    add_branch_trajectory(
      branch_network = branch_network,
      branches = branches,
      branch_progressions = branch_progressions
    ) %>%
    add_expression(
      counts = counts,
      expression = expression
    )

  # save dataset
  save_dataset(dataset, design_row$dataset_id)

  TRUE
})


