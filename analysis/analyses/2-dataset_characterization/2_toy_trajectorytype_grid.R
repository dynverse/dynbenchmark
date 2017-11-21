library(tidyverse)
library(PRISM)

rules_df <- crossing(
  max_degree = c("=3", ">3"),
  num_begin_nodes = c("=1", ">1"),
  num_end_nodes = c("=0", "=1", ">1"),
  num_intermediate_nodes = c("=1", ">1"),
  num_divergences = c("=0", "=1", ">1"),
  num_convergences = c("=0", "=1", ">1"),
  num_cycles = c("=0", ">0")
)

rules_df <- rules_df %>% filter(
  !(num_end_nodes == "=0" & num_cycles == "=0"),
  !(num_divergences == "=0" & num_convergences == "=0"),
  num_begin_nodes != "=1" | num_divergences != "=0" | (num_convergences != "=0" & num_cycles != "=0")
)

determine_props <- function(gr) {
  degr_in <- igraph::degree(gr, mode = "in")
  degr_out <- igraph::degree(gr, mode = "out")
  degr_tot <- degr_in + degr_out

  is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))
  # is_loop <- diag(as.matrix(igraph::as_adjacency_matrix(gr)))

  max_degree <- max(degr_tot)
  is_begin <- degr_in == is_loop & degr_out != 0
  is_end <- degr_in != 0 & degr_out == 0
  is_intermediate <- !is_begin & !is_end
  num_begin_nodes <- sum(is_begin)
  num_end_nodes <- sum(is_end)
  num_intermediate_nodes <- sum(is_intermediate)

  num_divergences <- sum(degr_in != 0 & degr_out > 1)
  num_convergences <- sum(degr_in > 1 & degr_out != 0)
  has_cycles <- !igraph::is.dag(gr)

  out <- c(
    max_degree = max_degree,
    num_begin_nodes = num_begin_nodes,
    num_end_nodes = num_end_nodes,
    num_intermediate_nodes = num_intermediate_nodes,
    num_divergences = num_divergences,
    num_convergences = num_convergences,
    num_cycles = has_cycles+0
  )
  attr(out, ".extra") <- tibble::lst(is_loop, is_begin, is_end, is_intermediate)
  out
}

simplify <- function(gr) {
  degr_in <- igraph::degree(gr, mode = "in")
  degr_out <- igraph::degree(gr, mode = "out")
  is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

  is_simple <- degr_in == 1 & degr_out == 1 & !is_loop
  while(any(is_simple)) {
    node <- first(which(is_simple))
    in_nodes <- igraph::neighbors(gr, node, mode = "in") %>% as.integer
    out_nodes <- igraph::neighbors(gr, node, mode = "out") %>% as.integer
    gr <- gr %>% igraph::delete.vertices(node) %>% igraph::add.edges(c(in_nodes, out_nodes))

    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

    is_simple <- degr_in == 1 & degr_out == 1 & !is_loop
  }

  gr
}

# bools <- sample(c(1, 0), nrow(poss_edges), replace = T)
fitness <- function(bools, rule_value, rule_geq, poss_gr) {
  gr <- igraph::subgraph.edges(poss_gr, eids = which(bools == 1))

  degr_in <- igraph::degree(gr, mode = "in")
  degr_out <- igraph::degree(gr, mode = "out")
  # is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))
  # is_removable <- degr_tot == 2 & !is_loop

  if (igraph::is_connected(gr) && !any(degr_in == 1 & degr_out == 1)) {
    props <- determine_props(gr)
    fits <- ifelse(rule_geq, props >= rule_value, props == rule_value)
    calcd <- -sum(ifelse(fits, 0, abs(props - rule_value)))
    if (calcd == 0) {
      mean(!bools)
    } else {
      calcd
    }
  } else {
    -5
  }
}


#rule_i <- 100
node_names <- letters[1:6]
poss_edges <- crossing(from = node_names, to = node_names)
poss_gr <- igraph::graph_from_data_frame(poss_edges, directed = T, vertices = node_names)
handle <- PRISM::qsub_lapply(
  qsub_config = PRISM::override_qsub_config(wait = F, remove_tmp_folder = F, max_wall_time = "12:00:00", memory = "1G"),
  X = seq_len(nrow(rules_df)),
  FUN = function(rule_i) {
    rule_row <- as.matrix(rules_df[rule_i,])
    rule_geq <- setNames(grepl(">", rule_row), colnames(rules_df))
    rule_value <- as.numeric(gsub("[=>]", "", rule_row)) + rule_geq

    ga_out <- GA::ga(
      type = "binary", fitness = fitness, nBits = nrow(poss_edges), popSize = 200, maxiter = 200,
      rule_value = rule_value, rule_geq = rule_geq, poss_gr = poss_gr)
    best_sol <- ga_out@solution[1,]
    gr <- igraph::subgraph.edges(poss_gr, eids = which(best_sol == 1))
    props <- determine_props(gr)
    fitns <- fitness(best_sol, rule_value, rule_geq, poss_gr)
    tibble::lst(rule_i, rule_value, rule_geq, ga_out, best_sol, gr, props, fitns)
  })

ga_outs <- PRISM::qsub_retrieve(handle) %>% dynutils::list_as_tibble()
# dir.create("analysis/data/derived_data/analyse_toys/", recursive = T)
# save(node_names, poss_edges, poss_gr, ga_outs, rules_df, determine_props, fitness, file = "analysis/data/derived_data/analyse_toys/data.RData")
load("analysis/data/derived_data/analyse_toys/data.RData")

working <- ga_outs %>% filter(fitns >= 0) %>% mutate(rule_sum = sapply(rule_value, sum)) %>% arrange(rule_sum)

working

autocurve.edges2 <- function (graph, start = 0.5) {
  cm <- igraph::count.multiple(graph)
  mut <- igraph::is.mutual(graph)  #are connections mutual?
  el <- apply(igraph::get.edgelist(graph, names = FALSE), 1, paste, collapse = ":")
  ord <- order(el)
  res <- numeric(length(ord))
  p <- 1
  while (p <= length(res)) {
    m <- cm[ord[p]]
    mut.obs <-mut[ord[p]] #are the connections mutual for this point?
    idx <- p:(p + m - 1)
    if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
      r <- 0
    } else {
      r <- seq(-start, start, length = m)
    }
    res[ord[idx]] <- r
    p <- p + m
  }
  res
}

pdf("analysis/figures/analyse_toys/plot.pdf", 10, 10)
for (i in seq_len(nrow(working))) {
  ls <- dynutils::extract_row_to_list(working, i)
  rule <- rules_df[ls$rule_i,]

  rule_str <- paste(paste0(names(rule), rule, c(rep(", ", 3), ",\n", rep(", ", 2), "")), collapse = "")
  gr <- ls$gr
  props <- ls$props
  node_props <- attr(props, ".extra")

  igraph::V(gr)$type <- with(node_props, ifelse(is_begin, "begin", ifelse(is_end, "end", "intermediate")))
  igraph::V(gr)$color <- c("begin"="green", "end"="red", "intermediate"="orange")[igraph::V(gr)$type]
  igraph::E(gr)$weight <- 1
  curves <- autocurve.edges2(gr)
  igraph::plot.igraph(gr, main = rule_str, edge.curved = curves, layout = igraph::layout_with_kk)
}
dev.off()
