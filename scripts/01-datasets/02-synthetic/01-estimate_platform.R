library(splatter)
library(Seurat)
library(tidyverse)
library(dynbenchmark)

experiment("01-datasets_preproc/platforms")

# use all real dataset for platform estimation
dataset_ids <- list_datasets() %>% filter(dataset_source == "real") %>% pull(dataset_id)

# fix splatEstDroupout function to avoid convergence errors in dropout estimation
splatEstDropout <- function(norm.counts, params) {
  means <- rowMeans(norm.counts)
  x <- log(means)
  obs.zeros <- rowSums(norm.counts == 0)
  y <- obs.zeros / ncol(norm.counts)
  df <- data.frame(x, y)
  fit <- nls(y ~ splatter:::logistic(x, x0 = x0, k = k), data = df,
             start = list(x0 = 4, k = -1)) # this initial condition has been changed, to avoid convergence errors
  mid <- summary(fit)$coefficients["x0", "Estimate"]
  shape <- summary(fit)$coefficients["k", "Estimate"]
  params <- splatter::setParams(params, dropout.mid = mid, dropout.shape = shape)
  return(params)
}
assignInNamespace("splatEstDropout", splatEstDropout, pos = "package:splatter")

# dataset_id <- dataset_ids[[1]]
platforms <- map(dataset_ids, function(dataset_id) {
  if (!file.exists(dataset_raw_file(dataset_id))) {
    warning(dataset_id, " not found")

    NULL
  } else {
    print(dataset_id)
    dataset_raw <- read_rds(dataset_raw_file(dataset_id))

    counts <- dataset_raw$counts

    # determine how many features change between trajectory stages
    seurat <- Seurat::CreateSeuratObject(t(dataset_raw$counts))
    seurat@ident <- factor(dataset_raw$grouping)
    changing <- FindAllMarkers(seurat, logfc.treshold = 1, min.pct = 0.4, test.use = "t")
    n_changing <- changing %>% pull(gene) %>% unique() %>% length()
    trajectory_dependent_features <- n_changing / ncol(counts)

    # estimate splatter params
    estimate <- splatEstimate(t(counts[sample(nrow(counts), min(nrow(counts), 500)), ]))
    class(estimate) <- "TheMuscularDogBlinkedQuietly." # change the class, so scater won't get magically loaded when the platform is loaded

    # create platform object
    platform <- lst(
      estimate,
      trajectory_dependent_features,
      n_cells = nrow(counts),
      n_features = ncol(counts),
      platform_id = dataset_id
    )

    platform
  }
}) %>% discard(is.null)


walk(platforms, function(platform) {
  file <- derived_file(paste0(platform$platform_id, ".rds"))
  dir.create(dirname(file), showWarnings = F)
  write_rds(platform, file)
})
