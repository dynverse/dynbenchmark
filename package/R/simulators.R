#' Simulating a trajectory dataset
#'
#' @rdname simulate_dataset
#' @name simulate_dataset
#'
#' @param topology_model The dyntoy model to use as baseline topology
#' @param platform The platform to use as reference
#' @param dataset_id The id of the dataset
#' @param n_steps_per_length Number of simulation steps per length unit (for splatter and prosstt)
#' @param seed The seed to use, will use the current seed if not given
#' @param use_cache Whether to allow the cache (stored in the dataset preprocessing source files)
NULL


#' @rdname simulate_dataset
#' @param path.skew Splatter parameter
#' @param path.nonlinearProb Splatter parameter
#' @param path.sigmaFac Splatter parameter
#' @param bcv.common.factor Splatter parameter
#'
#' @importFrom qsub rm_remote
#' @export
simulate_splatter <- function(
  dataset_id,
  topology_model = "linear",
  platform = dyngen::platform_simple(),
  n_steps_per_length = 100,
  path.skew = runif(1, 0, 1),
  path.nonlinearProb = runif(1, 0, 1),
  path.sigmaFac = runif(1, 0, 1),
  bcv.common.factor = runif(1, 10, 200),
  seed = NULL,
  use_cache = TRUE
) {
  simulation_design <- as.list(environment())

  requireNamespace("splatter")

  if (missing(dataset_id)) stop("dataset_id is required")
  dataset_preprocessing(dataset_id)

  # if cache disallowed, clear cache files
  if (!use_cache) {
    qsub::rm_remote(dataset_source_file(), remote = NULL, recursive = TRUE, force = TRUE)
  }

  if (!is.null(seed)) set.seed(seed)

  # simulate splatter
  sim <- load_or_generate(
    dataset_source_file("sim.rds"),
    {
      # get splatter parameters
      splatter_params <- platform$estimate
      class(splatter_params) <- "SplatParams"

      # extract path from milestone network
      milestone_network <- dyntoy::generate_milestone_network(topology_model)

      root <- setdiff(milestone_network$from, milestone_network$to)
      path.to <- c(root, milestone_network$to)
      path.from <- as.numeric(factor(milestone_network$from, levels = path.to)) - 1

      # factor added to bcv.common, influences how strong the biological effect is
      splatter_params@bcv.common <- splatter_params@bcv.common / bcv.common.factor

      # simulate
      sim <- splatter::splatSimulatePaths(
        splatter_params,
        batchCells = platform$n_cells,
        nGenes = platform$n_features,
        group.prob = milestone_network$length/sum(milestone_network$length),
        path.from = path.from,
        path.length = ceiling(milestone_network$length*n_steps_per_length),
        path.nonlinearProb = path.nonlinearProb,
        path.sigmaFac = path.sigmaFac,
        path.skew = path.skew
      )

      sim
    }
  )

  # get counts
  counts <- t(SingleCellExperiment::counts(sim))
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
    id = dataset_id,
    cell_ids = rownames(expression),
    source = "synthetic/splatter"
  ) %>%
    add_expression(
      counts = counts,
      expression = expression
    ) %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    ) %>%
    add_prior_information() %>%
    add_cell_waypoints()

  # add information on the simulation itself
  dataset$simulation_design <-
    list(
      simulator = "splatter",
      simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyntoy", "splatter", "dynnormaliser", "dynbenchmark"))
    ) %>% c(simulation_design)

  # save dataset
  save_dataset(dataset, dataset_id)
  dataset
}

#' @param a PROSSTT param
#' @param intra_branch_tol PROSSTT param
#' @param inter_branch_tol PROSSTT param
#' @param alpha PROSSTT param
#' @param beta PROSSTT param
#'
#' @rdname simulate_dataset
#' @export
simulate_prosstt <- function(
  dataset_id,
  topology_model = "linear",
  platform = dyngen::platform_simple(),
  n_steps_per_length = 100,
  a = as.integer(round(runif(1, 1, 10))),
  intra_branch_tol = runif(1, 0, 0.9),
  inter_branch_tol = runif(1, 0, 0.9),
  alpha = exp(rnorm(1, log(0.2), log(1.5))),
  beta = exp(rnorm(1, log(1), log(1.5))) + 1,
  seed = NULL
) {
  simulation_design <- as.list(environment())

  if (missing(dataset_id)) stop("dataset_id is required")

  # pip3 install git+https://github.com/soedinglab/prosstt
  # based on https://github.com/soedinglab/prosstt/blob/master/examples/generate_simN.py

  # load prosstt python package
  requireNamespace("reticulate")

  tryCatch({
    reticulate::use_python(system("which python3", intern=TRUE))

    tree <- reticulate::import("prosstt.tree")
    sim <- reticulate::import("prosstt.simulation")
    sut <- reticulate::import("prosstt.sim_utils")
  },
  error = function(e) {
    stop("PROSSTT seems not to be correctly installed, run pip3 install git+https://github.com/soedinglab/prosstt ", e)
  })

  # set seed
  if (!is.null(seed)) reticulate::py_set_seed(seed)

  # generate milestone network
  milestone_network <- dyntoy::generate_milestone_network(topology_model)

  # special case for the A->B network
  if (nrow(milestone_network) == 1) {
    milestone_network <- dyntoy::generate_milestone_network("linear", num_milestones = 3)
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
  topology <- branch_network %>% purrr::pmap(function(from, to) c(from, to))
  num_branches <- nrow(branches)
  time <- branches %>% select(branch_id, length) %>% mutate(length = as.integer(pmax(2, ceiling(length * n_steps_per_length)))) %>% deframe() %>% as.list()

  # construct tree
  n_features <- min(1500L, platform$n_features) # limit number of features, because of extreme memory issues when simulating more than 2000 features
  t <- tree$Tree(topology, G = as.integer(platform$n_features), num_branches = num_branches, time = time, root = root)

  # simulate expression across lineage
  lin <- sim$simulate_lineage(
    t,
    a = a,
    intra_branch_tol = intra_branch_tol,
    inter_branch_tol = inter_branch_tol
  )
  uMs <- lin[[1]]
  gene_scale = sut$simulate_base_gene_exp(t, uMs)

  Ms <- map(branches$branch_id, function(branch) exp(uMs[[branch]]) %*% diag(gene_scale)) %>% set_names(branches$branch_id)
  t$add_genes(Ms)

  # actual simulation
  simulation <- sim$sample_density(t, no_cells = as.integer(platform$n_cells), alpha = alpha, beta = beta)

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
  counts[counts < 0] <- 0 # in some cases, prosstt produces verry low numbers (eg. -8e54)

  counts <- counts[apply(counts, 1, max) > 0, ]
  if (nrow(counts) == 0) {stop("PROSSTT generated out-of-bound counts")}

  normalised <- dynnormaliser::normalise_filter_counts(counts, verbose = TRUE)
  counts <- normalised$counts
  expression <- normalised$expression

  branch_progressions <- branch_progressions %>% filter(cell_id %in% rownames(counts))

  # create dataset
  dataset <- wrap_data(
    id = dataset_id,
    cell_ids = rownames(counts),
    source = "synthetic/prosstt"
  ) %>%
    add_branch_trajectory(
      branch_network = branch_network,
      branches = branches,
      branch_progressions = branch_progressions
    ) %>%
    add_expression(
      counts = counts,
      expression = expression
    ) %>%
    add_prior_information() %>%
    add_cell_waypoints()

  dataset$simulation_design <-
    list(
      simulator = "prosstt",
      simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyntoy", "prosstt", "splatter", "dynbenchmark", "dynnormaliser"))
    ) %>% c(simulation_design)

  # save dataset
  save_dataset(dataset, dataset_id)
  dataset
}



#' @inheritParams dyntoy::generate_dataset
#'
#' @param count_mean_shape The shape of the gamma distribution from which the mean counts will be sampled
#' @param count_mean_scale The scale of the gamma distribution from which the mean counts will be sampled
#'
#' @rdname simulate_dataset
#' @export
simulate_dyntoy <- function(
  dataset_id,
  topology_model = "linear",
  platform = dyngen::platform_simple(),
  count_mean_shape = runif(1, 1, 10),
  count_mean_scale = runif(1, 1, 10),
  dropout_probability_factor = runif(1, 10, 200),
  seed = NULL
) {
  simulation_design <- as.list(environment())

  if (missing(dataset_id)) stop("dataset_id is required")

  if (!is.null(seed)) set.seed(seed)

  if (platform$estimate@mean.shape / platform$estimate@mean.rate < 1) {
    shape <- platform$estimate@mean.shape / platform$estimate@mean.rate
  } else {
    shape <- platform$estimate@mean.shape
  }

  # sample_mean_count <- function() rgamma(1, shape = shape, rate = platform$estimate@mean.rate)
  sample_mean_count <- function() rgamma(1, shape = count_mean_shape, scale = count_mean_scale)
  sample_dispersion_count = function(mean) map_dbl(mean, ~runif(1, ./10, ./4))

  dataset <- dyntoy::generate_dataset(
    dataset_id,
    model = topology_model,
    num_cells = platform$n_cells,
    num_features = ceiling(platform$n_features * platform$trajectory_dependent_features),
    sample_mean_count = sample_mean_count,
    sample_dispersion_count = sample_dispersion_count,
    dropout_probability_factor = dropout_probability_factor
  )

  dataset$source <- "synthetic/dyntoy"

  dataset$simulation_design <- list(
    simulator = "dyntoy",
    simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyntoy", "splatter", "dynbenchmark", "dynnormaliser"))
  ) %>% c(simulation_design)

  # add cell waypoints
  dataset <- dataset %>% dynwrap::add_cell_waypoints()

  # save dataset
  save_dataset(dataset, dataset_id)
  dataset
}




# #' @inheritParams dyngen::generate_model_from_modulenet
# #'
# #' @rdname simulate_dataset
# #'
# #' @importFrom dyngen get_simple_platform base_params generate_model_from_modulenet simulate_multiple extract_goldstandard run_experiment
# #' @export
# simulate_dyngen <- function(
#   dataset_id,
#   modulenet_name = "linear",
#   platform = dyngen::platform_simple(),
#   use_cache = TRUE,
#   seed = NULL
# ) {
#   simulation_design <- as.list(environment())
#'
#   if (missing(dataset_id)) stop("dataset_id is required")
#   dataset_preprocessing(dataset_id)
#'
#   # if cache disallowed, clear cache files
#   if (!use_cache) {
#     qsub::rm_remote(dataset_source_file(), remote = NULL, recursive = TRUE, force = TRUE)
#   }
#'
#   if (!is.null(seed)) set.seed(seed)
#'
#   # generate dyngen params
#   params <- dyngen::base_params
#   params$model$modulenet_name <- modulenet_name
#   params$model$platform <- platform
#   params$experiment$platform <- platform
#'
#   # generate model
#   model <- load_or_generate(
#     dataset_source_file("model.rds"),
#     invoke(dyngen::generate_model_from_modulenet, params$model)
#   )
#'
#   # simulate model
#   simulation <- load_or_generate(
#     dataset_source_file("simulation.rds"),
#     invoke(dyngen::simulate_multiple, params$simulation, model$system)
#   )
#'
#   # extract gold standard
#   gs <- load_or_generate(
#     dataset_source_file("gs.rds"),
#     invoke(dyngen::extract_goldstandard, params$gs, model = model, simulation = simulation)
#   )
#'
#   # generate experiment
#   experiment <- load_or_generate(
#     dataset_source_file("experiment.rds"),
#     invoke(dyngen::run_experiment, params$experiment, simulation=simulation, gs=gs)
#   )
#'
#   # normalise
#   normalisation <- load_or_generate(
#     dataset_source_file("normalisation.rds"),
#     invoke(dynnormaliser::normalise_filter_counts, params$normalisation, experiment$counts)
#   )
#'
#   # generate dynwrap dataset
#   dataset <- load_or_generate(
#     dataset_source_file("dataset.rds"),
#     dyngen::wrap_dyngen_dataset(dataset_id, params, model, simulation, gs, experiment, normalisation)
#   )
#   dataset$source <- "synthetic/dyngen"
#   dataset$simulation_design <- list(
#     simulator = "dyngen",
#     simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyngen","splatter", "dynbenchmark"))
#   ) %>% c(simulation_design)
#'
#   # add cell waypoints
#   dataset <- dataset %>% dynwrap::add_cell_waypoints()
#'
#   # save dataset
#   save_dataset(dataset, dataset_id)
#   dataset
# }
