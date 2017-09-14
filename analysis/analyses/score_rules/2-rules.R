scores <- read_rds("scores.rds")
toys <- read_rds("toys.rds")

source("scripts/wouter/toy/rules.R")

# summarise the scores
# get gs-toy score
# get difference and fractions between gs-toy and gs-gs
scores_summary <- scores %>% gather(score_id, score, -toy_id, -comparison) %>%
  spread(comparison, score) %>% mutate(
    score=`gs-toy`,
    diff= `gs-toy`-`gs-gs`,
    frac=`gs-toy`/`gs-gs`
  ) %>% select(-`gs-toy`, -`gs-gs`) %>% left_join(toys, by="toy_id")

scores_summary %>%
  ggplot() +
  geom_boxplot(aes(toy_category, score, color=perturbator_id)) +
  facet_wrap(~score_id) +
  coord_flip()

scores_summary %>%
  ggplot() +
  geom_boxplot(aes(toy_category, diff, color=perturbator_id)) +
  facet_wrap(~score_id) +
  coord_flip()


library(googlesheets)
rules_info <- gs_title("Metrics rules") %>% gs_read() %>% mutate(id=factor(id, levels=id))
rules_info$check_func <- map(paste0("check_", rules_info$id), get)

scores <- read_rds("scores.rds")

rules <- map2(
  rules_info$check_func,
  rules_info$id,
  ~.(scores_summary) %>%
    mutate(rule_id=.y)
  ) %>% bind_rows() %>% left_join(rules_info, by=c("rule_id"="id"))

## Plot all rules
rules %>% ggplot(aes(rule_id, score_id)) +
  geom_raster(aes(fill = c("red", "green")[as.numeric(rule)+1])) +
  geom_text(aes(label = c("▼", "▲")[as.numeric(rule)+1]), color="white") +
  scale_fill_identity()


## Possible score combinations satisfying all rules
allscores <-unique(rules$score_id)
score_ids_combinations <- map(1:length(allscores), function(x) map(combn(length(allscores), x, simplify = FALSE), ~allscores[.])) %>% unlist(recursive=FALSE)

map(score_ids_combinations, function(score_ids_combination) {
  rules %>% filter(score_id %in% score_ids_combination) %>%
    group_by(rule_id) %>%
    summarise(any=any(rule), number=sum(rule, na.rm=TRUE)) %>%
    group_by() %>%
    summarise(all=all(any), avg_rules_retrieved=sum(number)/length(score_ids_combination), nscores = length(score_ids_combination)) %>%
    mutate(score_ids_combination=list(score_ids_combination))
}) %>% bind_rows() %>% filter(all) %>% arrange(nscores, -avg_rules_retrieved) %>% View
