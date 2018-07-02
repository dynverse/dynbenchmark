library(reticulate)
library(tidyverse)
library(dynwrap)

# git clone https://github.com/soedinglab/prosstt.git
# cd prosstt
# sudo pip install .
# sudo pip install newick

tree <- import("prosstt.tree")
simulation <- import("prosstt.simulation")

num_cells <- 1000L
num_genes <- 1000L

topology <- dyntoy::generate_milestone_network("linear") %>% pmap(function(from, to, ...) list(from, to))
t <- tree$Tree(topology, G = num_genes)

sim <- simulation$sample_whole_tree_restricted(t) # you have to do this first, and then you can sample_density :p
sim <- simulation$sample_density(t, num_cells)
counts <- sim[[1]]
pseudotimes <- sim[[2]]
branch <- sim[[3]]
scalings <- as.numeric(sim[[4]])

cell_ids <- paste0("C", seq_len(nrow(counts)))
feature_ids <- paste0("G", seq_len(ncol(counts)))
dimnames(counts) <- list(cell_ids, feature_ids)

expression <- counts/scalings

branch_network <- tibble(
  from = t$topology %>% map_chr(first),
  to = t$topology %>% map_chr(last)
)

branches <- tibble(
  branch_id = t$branches,
  length = t$time,
  directed = TRUE
)

branch_progressions <- tibble(
  cell_id = cell_ids,
  branch_id = branch,
  percentage = pseudotimes
) %>%
  group_by(branch_id) %>%
  mutate(
    percentage = (percentage - min(percentage))/(max(percentage) - min(percentage))
  ) %>%
  ungroup()

# build dynwrap model
model <- wrap_data(
  "...",
  cell_ids,
) %>%
  add_expression(
    counts = counts,
    expression = expression
  ) %>%
  add_branch_trajectory(
    branch_network = branch_network,
    branches = branches,
    branch_progressions = branch_progressions
  )


dynplot::plot_dimred(model, dimred=dyndimred::dimred_tsne)

dynplot::plot_heatmap(model)
