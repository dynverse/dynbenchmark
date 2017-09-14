equal_tol <- function(a, b, tol = 1e-5) {abs(a-b) < tol}

gs_perturbator_ids <- c("gs", "switch_0")

# equal_gold_standard_score ----------------------------------------
check_equal_gold_standard_score <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "gs") %>%
    group_by(score_id) %>%
    summarise(rule = equal_tol(max(score), min(score)))
}

plot_equal_gold_standard_score <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "gs") %>%
    ggplot() +
    ggbeeswarm::geom_beeswarm(aes(score_id, score, color=toy_category))
}

# direct_gs_comparison -------------------------------------
check_direct_gs_comparison <- function(scores_summary) {
  scores_summary %>%
    filter(!(perturbator_id %in% gs_perturbator_ids)) %>%
    group_by(score_id) %>%
    summarise(rule = all(diff < 0))
}

plot_direct_gs_comparison <- function(scores_summary) {
  scores_summary %>%
    ggplot() +
    ggbeeswarm::geom_beeswarm(aes(toy_category, diff, color=perturbator_id)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~score_id)
}

# indirect_gs_comparison -----------------------------------
check_indirect_gs_comparison <- function(scores_summary) {
  scores2_summary %>%
    mutate(is_gs = (perturbator_id %in% gs_perturbator_ids)) %>%
    group_by(score_id) %>%
    summarise(
      maxdiff = max(score[!is_gs]) - min(score[is_gs]),
      rule = maxdiff <= 1e-10
    )
}

plot_indirect_gs_comparison <- function(scores_summary) {
  scores_summary %>%
    mutate(is_gs = (perturbator_id %in% gs_perturbator_ids)) %>%
    ggplot() +
    geom_boxplot(aes(is_gs, diff, color=toy_category)) +
    facet_wrap(~score_id)

  scores_summary %>%
    mutate(is_gs = (perturbator_id %in% gs_perturbator_ids)) %>%
    ggplot() +
    geom_boxplot(aes(is_gs, diff, color=perturbator_id)) +
    facet_wrap(~score_id)
}

# break_cycles ----------------------
check_break_cycles <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "break_cycles") %>%
    group_by(score_id) %>%
    summarise(rule_id="3a", rule = all(diff < 0))
}

# join_linear --------------------
check_join_linear <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "join_linear") %>%
    group_by(score_id) %>%
    summarise(rule = all(diff < 0))
}

# shuffle_two_vs_shuffle_all --------------------
check_shuffle_two_vs_shuffle_all <- function(scores_summary) {
  scores_summary_largevssmall <- scores_summary %>%
    group_by(generator_id) %>%
    filter(perturbator_id %in% c("switch_two_cells", "switch_all_cells")) %>%
    filter(length(unique(perturbator_id)) == 2) %>%
    ungroup() %>%
    mutate(is_small = (perturbator_id == "switch_two_cells"))

  scores_summary_largevssmall %>%
    group_by(score_id) %>%
    summarise(
      maxdiff = max(score[!is_small]) - min(score[is_small]),
      rule = all(maxdiff < 0)
    )
}

plot_shuffle_two_vs_shuffle_all <- function(scores_summary) {
  scores_summary_largevssmall %>% ggplot() +
    geom_boxplot(aes(toy_category, score, color=perturbator_id)) +
    facet_wrap(~score_id)
}

# hairy --------------------------
check_hairy <- function(scores_summary) {
  scores_summary_largevssmall <- scores_summary %>%
    group_by(generator_id) %>%
    filter(perturbator_id %in% c("hairy_small", "hairy_large")) %>%
    filter(length(unique(perturbator_id)) == 2) %>%
    ungroup() %>%
    mutate(is_small = (perturbator_id == "hairy_small"))

  scores_summary_largevssmall %>%
    group_by(score_id) %>%
    summarise(
      maxdiff = max(score[!is_small]) - min(score[is_small]),
      rule = all(maxdiff < 0)
    )
}

plot_hairy <- function(scores_summary) {
  scores_summary_largevssmall %>% ggplot() +
    geom_boxplot(aes(toy_category, score, color=perturbator_id)) +
    facet_wrap(~score_id)
}

# split_linear ------------------------------
check_split_linear <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "split_linear") %>%
    group_by(score_id) %>%
    summarise(rule = all(diff < 0))
}

# warping --------------------------------
check_warping <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "warp") %>%
    group_by(score_id) %>%
    summarise(rule = all(diff < 0))
}

plot_warping <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "warp") %>%
    ggplot() +
    ggbeeswarm::geom_beeswarm(aes(score_id, diff, color=generator_id))
}


# hairy_large_effect --------------------------------
check_hairy_large_effect <- function(scores_summary) {
  scores_summary %>%
    group_by(score_id) %>%
    mutate(maxscore = max(score)) %>%
    filter(perturbator_id == "hairy") %>%
    summarise(rule = all(score < maxscore/2))
}


# defined -------------------------------
check_defined <- function(scores_summary) {
  scores_summary %>%
    group_by(score_id) %>%
    summarise(rule = all(!is.na(score)))
}



# linear ----------------------------------
check_linear <- function(scores_summary) {
  scores_summary %>%
    filter(grepl("^switch_\\d*$", perturbator_id)) %>%
    mutate(perc = as.numeric(gsub("switch_(\\d*)", "\\1", perturbator_id))) %>%
    mutate(score = score + runif(n(), 0, 1e-10)) %>%
    group_by(score_id) %>%
    summarise(cor=cor(perc, score), rule = cor < -0.9)
}



# remove_cells --------------------------
check_remove_cells <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "remove_cells") %>%
    group_by(score_id) %>%
    summarise(rule=all(diff < 0))
}



# different_structure
check_different_structure <- function(scores_summary) {
  scores_summary %>%
    filter(perturbator_id == "add_distant_edge") %>%
    group_by(score_id) %>%
    summarise(rule=all(diff < 0))
}


toy <- toys %>% filter(perturbator_id=="hairy") %>% .$toy %>% first
milestone_ids <- toy$milestone_percentages %>% group_by(cell_id) %>% summarise(list(milestone_id)) %>% pull(2) %>% keep(~length(.) > 1)

map(milestone_ids, ~any((toy$milestone_network$from == .[[1]]) & (toy$milestone_network$to == .[[2]])))
toy$milestone_network
