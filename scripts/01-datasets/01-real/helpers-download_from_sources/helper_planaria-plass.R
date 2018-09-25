# individual trajectories after the neoblast stage
settings_individual <- list(
  list(
    subid = "epidermis-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "epidermal neoblasts", "early epidermal progenitors",
      "early epidermal progenitors", "late epidermal progenitors 1",
      "late epidermal progenitors 1", "late epidermal progenitors 2",
      "late epidermal progenitors 2", "epidermis"
    )
  ),
  list(
    subid = "parenchyme-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "parenchymal progenitors", "pigment",
      "parenchymal progenitors", "aqp+ parenchymal cells",
      "parenchymal progenitors", "psap+ parenchymal cells",
      "parenchymal progenitors", "ldlrr-1+ parenchymal cells"
    )
  ),
  list(
    subid = "neuron-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "neural progenitors", "ChAT neurons 1",
      "neural progenitors", "ChAT neurons 2",
      "neural progenitors", "spp-11+ neurons",
      "neural progenitors", "npp-18+ neurons",
      "neural progenitors", "GABA neurons",
      "neural progenitors", "cav-1+ neurons"
    )
  ),
  list(
    subid = "muscle-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "muscle progenitors", "muscle body",
      "muscle progenitors", "muscle pharynx"
    )
  ),
  list(
    subid = "pharynx-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "pharynx cell type progenitors", "pharynx cell type"
    )
  ),
  list(
    subid = "phagocyte-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "gut progenitors", "phagocytes"
    )
  )
)

settings_individual <- map(settings_individual, function(setting) {
  setting$id <- paste0("real/silver/planaria-", setting$subid, "_plass")
  setting
})

# combine different disconnected stages
set.seed(1)
combinations <- c(
  combn(seq_along(settings_individual), 2, simplify=F) %>% sample(8),
  combn(seq_along(settings_individual), 3, simplify=F) %>% sample(4),
  combn(seq_along(settings_individual), 4, simplify=F) %>% sample(2)
)

settings_combinations <-
  map2(seq_along(combinations), combinations, function(combination_ix, combination) {
    setting <- list(
      milestone_network = bind_rows(map(settings_individual[combination], "milestone_network")),
      id = paste0("real/silver/planaria-combination-", combination_ix, "_plass"),
      progenitors = map_chr(settings_individual[combination], ~.$milestone_network$from[[1]])
    )
  })

# combine to neoblast to define more complex tree trajectories
settings_pairs <-
  map2(seq_along(settings_combinations), settings_combinations, function(combination_ix, setting_combination) {
    setting <- list()

    setting$milestone_network  <- setting_combination$milestone_network %>% bind_rows(
      tibble(from = "neoblast 1", to = setting_combination$progenitors),
      .
    )

    setting$id <- paste0("real/silver/planaria-pair-", combination_ix, "_plass")

    setting
  })

# full planaria
additional_endstates <- c("otf+ cells 1", "otf+ cells 2", "secretory 2", "secretory 1", "secretory 3", "secretory 4", "goblet cells", "protonephridia")
setting_full <- list(
  id = "real/silver/planaria-full_plass",
  milestone_network = map_dfr(settings_individual, function(setting) {
    milestone_network <- setting$milestone_network %>% bind_rows(
      tibble(from = "neoblast 1", to = setting$milestone_network$from[[1]]),
      .
    )
    milestone_network
  }) %>% bind_rows(
    tibble(
      from = "neoblast 1", to = additional_endstates
    )
  )
)



# combine all settings
settings <- c(
  settings_individual,
  settings_combinations,
  settings_pairs,
  list(setting_full)
)
