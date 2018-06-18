# code to generate the issues for authors on the dynmethods github

library(dynalysis)
library(tidyverse)
library(cowplot)
library(googlesheets)

experiment("4-method_characterisation")


#   ____________________________________________________________________________
#   Update QC at google sheets                                              ####
# update the public quality control values at https://docs.google.com/spreadsheets/d/1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo/edit#gid=174940161

implementation_qc <- readRDS(derived_file("implementation_qc.rds"))

qc_ss <- gs_key("1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo")
worksheets <- gs_ws_ls(qc_ss)

implementation_ids <- unique(implementation_qc$implementation_id)
implementation_ids <- "scorpius"

for (implementation_id in implementation_ids) {
  input <-  implementation_qc %>%
    filter(implementation_id == !!implementation_id) %>%
    select(item, answer, note=answer_description)

  if(!implementation_id %in% worksheets) {
    gs_ws_new(qc_ss, implementation_id, input=input, trim=TRUE)
  } else {
    gs_edit_cells(qc_ss, implementation_id, input = input, trim = TRUE)
  }
}


#   ____________________________________________________________________________
#   Create issue from blueprint                                             ####

github_url <- function(x) paste0("https://github.com/dynverse/dynmethods/tree/master/", x)

methods <- readRDS(derived_file("methods.rds"))
implementations <- readRDS(derived_file("implementations.rds"))

# choose implementation
implementation_id <- "scorpius"
implementation <- implementations %>% filter(implementation_id == !!implementation_id) %>% extract_row_to_list(1)

# get methods of this implementation
methods_oi <- methods %>% filter(implementation_id == !!implementation_id)

# create text
wrapper <- if (!first(methods_oi$r_wrapped)) {
  glue::glue("{implementation$implementation_name} was wrapped within a [docker container]({github_url(methods_oi$docker_wrapper_location[[1]])}). The way this container is structured is further described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html).")
} else {
  glue::glue("{implementation$implementation_name} was [wrapped within R]({github_url(methods_oi$r_wrapper_location[[1]])}). This same wrapper was also put into a [docker container]({github_url(methods_oi$docker_wrapper_location[[1]])}).")
}

implementation_information <- if(nrow(methods_oi) == 1) {
  ""
} else {
  if (any(is.na(methods_oi$method_note))) {stop("Need a note when having multiple methods")}
  c(
    glue::glue("We created {nrow(methods_oi)} separate wrappers:"),
    glue::glue("- [**{methods_oi$method_name}**]({github_url(methods_oi$wrapper_location)}): {methods_oi$method_note}")
  ) %>% glue::collapse("\n")
}

created_by_text <- if(implementation$contact_github != ", ") {
  paste0(
    ", a TI method created by ",
    implementation$contact_github %>%
      str_split(", ") %>%
      unlist() %>%
      paste0("@", .) %>%
      glue::collapse(", ", last=" and ") %>%
      paste0(., ".")
  )
} else {
  "."
}

qc_ss <- gs_key("1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo")
qc_worksheet_url <- qc_ss$ws %>%
  filter(ws_title == implementation_id) %>%
  pull(gid) %>%
  paste0(qc_ss$browser_url, "/edit#gid=", .)
if (length(qc_worksheet_url) == 0) {stop("Implementation not found on google sheet")}

blueprint <- glue::glue("
Checklist & discussion on the [{implementation$implementation_name}]({implementation$code_location}) wrappers{created_by_text}

{wrapper}

{implementation_information}

Following aspects of the wrapper still have to be confirmed with the authors:

- [ ] Parameters
    - Are all important parameters exposed?
    - Are the correct default values used?
    - Is the parameter space reasonable (the lower and upper boundaries for numeric parameters, the possible values for discrete parameters) ?
    - Is the documentation of the parameters correct and up-to-date?
- [ ] Input
    - Is the correct type of expression requested (raw counts or normalised expression)?
    - Is all prior information (required or optional) requested?
    - Would some other type of prior information help the method?
- [ ] Wrapper
    - Are all important steps run?
    - Are some steps unnecessary?
    - Is some additional parameter checking necessary to avoid errors?
- [ ] Output
    - Is the output correctly processed towards the [common trajectory model](https://github.com/dynverse/dynwrap#dynwrap)? Would some other postprocessing method make more sense?
    - Is all relevant output saved (dimensionality reduction, clustering/grouping, pseudotime, ...)
- [ ] Quality control
    - Is the [quality control assessment]({qc_worksheet_url}) of the wrapper correct and up to date?
    - You can improve the QC score of your method by implementing the required changes and letting us know here
- [ ] Are there any other comments?

We welcome further feedback.
")

blueprint %>% clipr::write_clip()




##  ............................................................................
##  Patch github issue                                                      ####

library(curl)
library(httr)
library(jsonlite)

# first authorise
oauth_endpoints("github")

app <- oauth_app(
  "github",
   key = Sys.getenv("GITHUB_DYNMETHODS_APP_KEY"),
   secret = Sys.getenv("GITHUB_DYNMETHODS_APP_SECRET")
)

github_token <- oauth2.0_token(oauth_endpoints("github"), app, scope="repo")

gtoken <- config(token = github_token)

handle <- handle("https://api.github.com")
issues_url <- "https://api.github.com/repos/dynverse/dynmethods/issues"

issues <- GET(
  issues_url,
  body = list(labels=c("method discussion"))
) %>% content() %>% list_as_tibble()

issue_number <- issues %>% filter(title == implementation$implementation_name) %>% pull(number)

if (length(issue_number) == 0) {
  stop("No issue found!")
} else if (length(issue_number) > 1) {
  stop("Multiple issues found!")
}

issue_url <- paste0(issues_url, "/", issue_number)

issue_patch <- list(
  body = blueprint
)
issue <- PATCH(
  issue_url,
  body = issue_patch,
  encode = "json",
  gtoken
)

