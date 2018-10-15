# Different topologies with the same number of milestones
topologies_with_same_n_milestones <- list(
  linear = tibble(from = c("A", "B", "C", "D", "E"), to = c("B", "C", "D", "E", "F")),
  bifurcation = tibble(from = c("A", "B", "B", "C", "D"), to = c("B", "C", "D", "E", "F")),
  multifurcating = tibble(from = c("A", "B", "B", "B", "C"), to = c("B", "C", "D", "E", "F")),
  tree = tibble(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F")),
  cycle = tibble(from = c("A", "B", "C", "D", "E"), to = c("B", "C", "D", "E", "A")),
  connected = tibble(from = c("A", "B", "B", "D", "E"), to = c("B", "C", "D", "E", "A")),
  disconnected = tibble(from = c("A", "B", "C", "D", "E"), to = c("B", "C", "A", "E", "F"))
)
