# code to generate the issues for authors on the dynmethods github

library(dynbenchmark)
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
# implementation_ids <- "ouijaflow"

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
library(curl)
library(httr)
library(jsonlite)

# get github issues
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
  paste0(issues_url, "?per_page=100"),
  body = list(labels=c("method discussion"))
) %>% content() %>% list_as_tibble()

# get qc spreadsheet
qc_ss <- gs_key("1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo")

# some helpers
github_url <- function(x) paste0("https://github.com/dynverse/dynmethods/tree/master/", x)
first_non_na <- function(x) {x %>% discard(is.na) %>% first}

# load function
methods <- readRDS(derived_file("methods.rds"))
implementations <- readRDS(derived_file("implementations.rds"))

# add [x] to implementations
n_xs <- 5
implementations_xs <- tibble(
  implementation_name = issues$title,
  xs = issues$body %>%
    map(~str_extract_all(., "\\[([ x])\\]")[[1]]) %>%
    map(~c(., rep("[ ]", n_xs - length(.))))
)
implementations <- left_join(implementations, implementations_xs, "implementation_name")
implementations$xs <- map(implementations$xs, function(xs) {
  if (any(is.na(xs))) {
    rep("[ ]", n_xs)
  } else {
    xs
  }
})


# choose implementations
implementation_ids <- implementations %>% filter(implementation_name %in% issues$title) %>% pull(implementation_id)
implementation_id <- "scorpius"

for (implementation_id in implementation_ids) {
  print(implementation_id)

  implementation <- implementations %>% filter(implementation_id == !!implementation_id) %>% extract_row_to_list(1)

  # get methods of this implementation
  methods_oi <- methods %>% filter(implementation_id == !!implementation_id)

  # create texts
  hello_text <- map_chr(implementation$authors, function(author) {
    if (!is.null(author$github)) {
      paste0("@", author$github)
    } else {
      author$given
    }
  }) %>% glue::collapse(", ", last = " and ")

  code_text <- if(nrow(methods_oi) == 1) {
    glue::glue("[a docker container]({github_url(methods_oi$docker_wrapper_location)})")
  } else {
    paste0(
      "docker containers",
      glue::collapse(
        glue::glue("[[{seq_len(nrow(methods_oi))}]]({github_url(methods_oi$docker_wrapper_location)})")
      )
    )
  }

  definition_text <- glue::glue("[definition.yml]({github_url(methods_oi$docker_definition_location %>% first_non_na)})")
  entrypoint_text <- glue::glue("[{basename(methods_oi$docker_entrypoint_location %>% first_non_na)}]({github_url(methods_oi$docker_entrypoint_location %>% first_non_na)})")

  implementation_information <- if(nrow(methods_oi) == 1) {
    ""
  } else {
    if (any(is.na(methods_oi$method_note))) {stop("Need a note when having multiple methods")}
    c(
      glue::glue("We created {nrow(methods_oi)} separate wrappers:"),
      glue::glue("- [**{methods_oi$method_name}**]({github_url(methods_oi$docker_wrapper_location)}): {methods_oi$method_note}")
    ) %>% glue::collapse("\n")
  }

  qc_worksheet_url <- qc_ss$ws %>%
    filter(ws_title == implementation_id) %>%
    pull(gid) %>%
    paste0(qc_ss$browser_url, "/edit#gid=", .)
  if (length(qc_worksheet_url) == 0) {stop("Implementation not found on google sheet")}
  qc_worksheet_text <- glue::glue("[the qc worksheet]({qc_worksheet_url})")

  blueprint <- glue::glue("
Hello {hello_text}

This issue is for discussing the wrapper for your trajectory inference method, {implementation$implementation_name}, which we wrapped for our benchmarking study ([10.1101/276907](https://doi.org/10.1101/276907)). In our [dynmethods](https://github.com/dynverse/dynmethods) framework, we collected some meta information about your method, and created a docker wrapper so that all methods can be easily run and compared. The code for this wrapper is located in {code_text}. The way this container is structured is described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html).

{implementation_information}

We are creating this issue to ensure your method is being evaluated in the way it was designed for. The checklist below contains some important questions for you to have a look at.

- {implementation$xs[[1]]} **Parameters**, defined in {definition_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#parameters))
  - Are all important parameters described in this file?
  - For each parameter, is the proposed default value reasonable?
  - For each parameter, is the proposed parameter space reasonable (e.g. lower and upper boundaries)?
  - Is the description of the parameters correct and up-to-date?
- {implementation$xs[[2]]} **Input**, defined in {definition_text} and loaded in {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#input))
  - Is the correct type of expression requested (raw counts or normalised expression)?
  - Is all prior information (required or optional) requested?
  - Would some other type of prior information help the method?
- {implementation$xs[[3]]} **Output**, defined in {definition_text} and saved in {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#output))
  - Is the output correctly processed towards the [common trajectory model](https://github.com/dynverse/dynwrap#dynwrap)? Would some other postprocessing method make more sense?
  - Is all relevant output saved (dimensionality reduction, clustering/grouping, pseudotime, ...)
- {implementation$xs[[4]]} **Wrapper script**, see {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#doing-trajectory-inference))
  - This is a script that is executed upon starting the docker container. It will receive several input files as defined by `definition.yml`, and is expected to produce certain output files, also as defined by `definition.yml`.
  - Is the script a correct representation of the general workflow a user is expected to follow when they want to apply your method to their data?
- {implementation$xs[[5]]} **Quality control**, see {qc_worksheet_text}
  - We also evaluated the implementation of a method based on a large check list of good software software development practices.
  - Are the answers we wrote down for your method correct and up to date? Do you disagree with certain answers? (Feel free to leave a comment in the worksheet)
    - You can improve the QC score of your method by implementing the required changes and letting us know. *Do not gloss over this, as it is the easiest way to improve the overall ranking of your TI method in our study!*

The most convenient way for you to test and adapt the wrapper is to install [dyno](https://github.com/dynverse/dyno), download and modify [these files]({github_url(methods_oi$docker_wrapper_location)[[1]]}), and run your method on a dataset of interest or one of our synthetic toy datasets. This is further described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html). Once finished, we prefer that you [fork the dynmethods repository](https://github.com/dynverse/dynmethods), make the necessary changes, and send us a pull request. Alternatively, you can also send us the files and we will make the necessary changes.

If you have any further questions or remarks, feel free to reply to this issue.

Kind regards,
{glue::collapse(sample(c('@rcannood', '@zouter')), ' and ')}
")

  blueprint %>% clipr::write_clip()

  ##  ............................................................................
  ##  Patch github issue                                                      ####

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
}



