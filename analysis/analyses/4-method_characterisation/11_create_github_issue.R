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

qc_ss <- gs_key("1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo")
qc_worksheet_url <- qc_ss$ws %>%
  filter(ws_title == implementation_id) %>%
  pull(gid) %>%
  paste0(qc_ss$browser_url, "/edit#gid=", .)
if (length(qc_worksheet_url) == 0) {stop("Implementation not found on google sheet")}

blueprint <- glue::glue("
Hello {hello_text}

This issue is for discussing the wrapper for your trajectory inference method, {implementation$implementation_name}, which we created for our benchmarking study ([10.1101/276907](https://doi.org/10.1101/276907)). In our [dynmethods](https://github.com/dynverse/dynmethods) framework, we collected some meta information about your method, and created a docker wrapper so that all methods can be easily run and compared. The code for this wrapper is located in {code_text}. The way this container is structured is described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html).

We are creating this issue to ensure your method is being evaluated in the way it was designed for. The checklist below contains some important questions for you to have a look at.

Referring to [definition.yml]({github_url(methods_oi$docker_definition_location[[1]])}):

- [ ] [Parameters](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#parameters)
    - Are all important parameters described in this file?
    - For each parameter, is the proposed default value reasonable?
    - For each parameter, is the proposed parameter space reasonable (e.g. lower and upper boundaries)?
    - Is the description of the parameters correct and up-to-date?
- [ ] [Input](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#input)
    - Is the correct type of expression requested (raw counts or normalised expression)?
    - Is all prior information (required or optional) requested?
    - Would some other type of prior information help the method?
- [ ] [Output](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#output)
    - Is the output correctly processed towards the [common trajectory model](https://github.com/dynverse/dynwrap#dynwrap)? Would some other postprocessing method make more sense?
    - Is all relevant output saved (dimensionality reduction, clustering/grouping, pseudotime, ...)

Referring to the [script executing the method]({github_url(methods_oi$docker_entrypoint_location[[1]])}):

- [ ] [Wrapper]({github_url(methods_oi$docker_entrypoint_location[[1]])})
    - This is a script that is executed upon starting the docker container. It will receive several input files as defined by `definition.yml`, and is expected to produce certain output files, also as defined by `definition.yml`.
    - Is the script a correct representation of the general workflow a user is expected to follow when they want to apply your method to their data?

Referring to the [quality control]({qc_worksheet_url}):

- [ ] [Quality control]({qc_worksheet_url})
    - We also evaluated the implementation of a method based on a large check list of good software software development practices.
    - Are the answers we wrote down for your method correct and up to date? Do you disagree with certain answers?
    - You can improve the QC score of your method by implementing the required changes and letting us know. **Do not gloss over this, as it is the easiest way to improve the overall ranking of your TI method in our study!**

The most convenient way for you to test and adapt the wrapper is to install [dyno](https://github.com/dynverse/dyno), download and modify [these files]({github_url(methods_oi$docker_wrapper_location)[[1]]}), and run your method on a dataset of interest or one of our synthetic toy datasets. This is further described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html). Once finished, we prefer that you [fork the dynmethods repository](https://github.com/dynverse/dynmethods), make the necessary changes, and send us a pull request. Alternatively, you can also send us the files and we will make the necessary changes.

If you have any further questions or remarks, feel free to reply to this issue.

Kind regards,
Robrecht and Wouter

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

