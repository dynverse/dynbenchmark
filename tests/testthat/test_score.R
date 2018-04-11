generate_blob_datasets <- function(means=c(1, 1), ncells=120, sd=1) {
  set.seed(1)
  ngroups <- nrow(means)
  milestone_network <- tibble(
    from=as.character(seq_len(ngroups)[-ngroups]),
    to=as.character(seq_len(ngroups)[-1])
  )
  milestone_ids <- as.character(seq_len(ngroups))

  cell_grouping <- tibble(cell_id=paste0("C", 1:ncells), group_id=as.character(rep(seq_len(ngroups), each=ncells/ngroups)))

  gene_means <- map(cell_grouping$group_id, ~means[as.numeric(.), ])

  counts <- map(gene_means, ~rnorm(50 * length(.), rep(., each=50), sd=sd)) %>% do.call(rbind, .)
  rownames(counts) <- cell_grouping$cell_id

  lst(milestone_network, milestone_ids, cell_grouping, counts)
}

plot_datasets <- function(datasets, color_by="group_id") {
  all_counts <- map(datasets, "counts") %>% do.call(rbind, .)
  all_cell_grouping <- map(seq_along(datasets), function(i) datasets[[i]]$cell_grouping %>% mutate(dataset_id = i)) %>% do.call(bind_rows, .)
  space <- dimred_pca(all_counts, 2) %>% as.data.frame()
  space <- bind_cols(space, all_cell_grouping)

  if(color_by == "group_id") {
    space$color <- space$group_id
  } else if (color_by == "ncounts") {
    space$color <- apply(all_counts, 1, sum)
  }
  ggplot(space) + geom_point(aes(Comp1, Comp2, color=color)) + facet_wrap(~dataset_id)
}

dimred_pca = function(x, ndim=3) {
  space = prcomp(t(x))$rotation[,seq_len(ndim)]
  space = as.matrix(space)
  colnames(space) = paste0("Comp", 1:ncol(space))
  space
}

context("Dataset scores")

verbose <- FALSE

test_that("Connectivity score", {
  ## connectivity -----------------------
  adjustments <- seq(-2, 4, 0.2)
  means <- adjustments %>% map(~list(c(1, 3, 1), c(1+., 3-., 1), c(3, 1, 1)) %>% do.call(rbind, .))
  datasets <- map(means, generate_blob_datasets, sd=2)

  if (verbose) plot_datasets(datasets)

  scores <- map_dbl(datasets, function(d) score_milestone_connectivity(d$counts, d$cell_grouping, d$milestone_ids, d$milestone_network, perc=0.2)$connectivity)
  expect_equal(which.max(scores), which(adjustments == 1))

  if (verbose) plot(scores)
})

test_that("Transitions score", {
  ## transitions ------------------------
  adjustments <- seq(-1, 3, 0.5)
  means <- adjustments %>% map(~list(c(1, 3, 1), c(1+., 3-., 1), c(3, 1, 1), c(3, 1, 2)) %>% do.call(rbind, .))
  datasets <- map(means, generate_blob_datasets, sd=2)

  if (verbose) plot_datasets(datasets)

  scores <- map(datasets, function(dataset) {
    counts_grouped <- dynwrap::group_counts(dataset$counts, dataset$cell_grouping)
    score_milestone_transitions(counts_grouped, dataset$milestone_ids, dataset$milestone_network)
  }) %>% bind_rows()
  expect_equal(which.max(scores$transition_frac), which(adjustments == 1))
  expect_equal(max(scores$transition_pval), scores$transition_pval[which(adjustments == 1)])

  if (verbose) plot(scores$transition_frac)
})

test_that("Grouping score", {
  ## grouping ---------------------------
  adjustments <- seq(0, 8, 1)
  means <- adjustments %>% map(~list(c(1, 3), c(1+., 3)) %>% do.call(rbind, .))
  datasets <- map(means, generate_blob_datasets, sd=2)

  if (verbose) plot_datasets(datasets)

  scores <- map(datasets, function(dataset) {
    score_milestone_grouping(dataset$counts, dataset$cell_grouping)
  }) %>% bind_rows()
  expect_equal(which.max(scores$grouping_asw), length(adjustments))

  if (verbose) plot(scores)
})

test_that("Aggregation score", {
  ## aggregation --------------------------
  adjustments <- seq(1, 4, 0.4)
  means <- adjustments %>% map(~list(c(1, 2, 3), c(1, 3, 2), c(2*., 3*., 1*.)) %>% do.call(rbind, .))
  datasets <- map(means, generate_blob_datasets, sd=2)

  if (verbose) plot_datasets(datasets, "ncounts")

  scores <- map_dbl(datasets, function(dataset) {
    score_aggregation(dataset$counts, dataset$cell_grouping)$aggregation_sd_frac
  })
  which.max(scores) == which(adjustments == 1)

  if (verbose) plot(scores)
})
