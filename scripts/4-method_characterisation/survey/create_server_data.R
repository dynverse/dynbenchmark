library(tidyverse)
library(googlesheets)
library(dynverse)

experiment("4-method_characterisation")
analysis_folder <- "analysis/analyses/4-method_characterisation"

checks <- readRDS(derived_file("checks.rds"))



addition_aspects_survey <- list(
  type = "paneldynamic",
  name = "additional",
  title = "Additional aspects",
  keyName = "name",
  showaspectNumbers = "none",
  templateTitle = "",
  templateElements = list(
    list(
      type = "text",
      name = "aspect",
      title = "aspect",
      isRequired = TRUE,
      startWithNewLine = FALSE
    ),
    list(
      type = "rating",
      name = "rating",
      title = "Rating",
      rateValues = c(0, 1, 2, 3, 4, 5),
      minRateDescription= "Not important",
      maxRateDescription= "Crucial",
      isRequired = TRUE,
      startWithNewLine = FALSE
    )
  )
)



create_checks_survey <- function(checks, title="Title") {
  survey_aspect_data <- checks %>% group_by(aspect_id) %>%
    summarise(
      survey_scoring_html = glue::collapse(map2(scoring, score, ~glue::glue("* {.y}: {.} \\n"))),
      survey_title = first(aspect),
      survey_description = glue::glue("{survey_scoring_html}"),
      category = first(category)
    ) %>%
    mutate(aspect_id = as.character(aspect_id))

  survey_aspects <- survey_aspect_data %>% as.list() %>% pmap(function(aspect_id, survey_title, survey_description, category, ...) {
    list(
      type = "rating",
      name = aspect_id,
      title = survey_title,
      description = survey_description,
      rateValues = c(0, 1, 2, 3, 4, 5),
      minRateDescription= "Not important",
      maxRateDescription= "Crucial",
      isRequired = TRUE,
      category = category
    )
  })

  survey_aspects <- c(survey_aspects, list(addition_aspects_survey))

  survey_json <-
    list(
      title = title,
      aspects = survey_aspects,
      requiredText = "",
      showaspectNumbers = "off"
    )
  survey_json
}

create_checks_survey(
  checks,
  "How important are these characteristics for both users and developers of bioinformatics tools?"
) %>% rjson::toJSON() %>% write(paste0(analysis_folder, "/survey/static/aspects_both.json"))

create_checks_survey(
  checks %>% filter(user_friendly),
  "How important are these characteristics for the users of bioinformatics tools?"
) %>% rjson::toJSON() %>% write(paste0(analysis_folder, "/survey/static/aspects_user.json"))

create_checks_survey(
  checks %>% filter(developer_friendly),
  "How important are these characteristics for developers of bioinformatics tools?"
) %>% rjson::toJSON() %>% write(paste0(analysis_folder, "/survey/static/aspects_developer.json"))


###
nonce <-sodium::hash(charToRaw(readr::read_file(paste0(analysis_folder, "/survey/nonce"))), size = 24)
passphrase <- sodium::hash(charToRaw(readr::read_file(paste0(analysis_folder, "/survey/passphrase"))))

library(sodium)

people <- tribble(
  ~id, ~category,
  "WouterSaelens", "developer",
  "RobrechtCannoodt", "developer",
  "LiesbetMartens", "user",
  "DanielPeralta", "both"
)

generate_key <- function(message, passphrase, nonce) {
  map_chr(message, ~glue::collapse(sodium::data_encrypt(charToRaw(.), passphrase, nonce = nonce)))
}

code_to_key <- function(code) {
  code %>% {substring(., seq(1, nchar(.), 2), seq(2, nchar(.), 2))} %>% as.hexmode() %>% as.raw()
}

people <- people %>% mutate(
  key = generate_key(id, passphrase, nonce)
)


people %>% readr::write_csv(paste0(analysis_folder, "/survey/people.csv"))
