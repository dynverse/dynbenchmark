#' Add & update the method discussion issues on the dynmethods github

library(dynbenchmark)
library(tidyverse)
library(cowplot)
library(googlesheets)

experiment("03-methods")


#   ____________________________________________________________________________
#   Update QC at google sheets                                              ####
# update the public quality control values at https://docs.google.com/spreadsheets/d/1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo/edit#gid=174940161

tool_qc <- readRDS(derived_file("tool_qc.rds"))

qc_ss <- gs_key("1GWWdndfTPtXPsCXK07jU03MmhIJHmv28E7YiMnQ_0Xo")
worksheets <- gs_ws_ls(qc_ss)

tool_ids <- unique(tool_qc$tool_id)
# tool_ids <- "ouijaflow"

for (tool_id in tool_ids) {
  input <-  tool_qc %>%
    filter(tool_id == !!tool_id) %>%
    select(item, answer, note=answer_description)

  if(!tool_id %in% worksheets) {
    gs_ws_new(qc_ss, tool_id, input=input, trim=TRUE)
  } else {
    gs_edit_cells(qc_ss, tool_id, input = input, trim = TRUE)
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
tools <- readRDS(derived_file("tools.rds"))

# add [x] to tools
n_xs <- 5
tools_xs <- tibble(
  tool_name = issues$title,
  xs = issues$body %>%
    map(~str_extract_all(., "\\[([ x])\\]")[[1]]) %>%
    map(~c(., rep("[ ]", n_xs - length(.))))
)
tools <- left_join(tools, tools_xs, "tool_name")
tools$xs <- map(tools$xs, function(xs) {
  if (any(is.na(xs))) {
    rep("[ ]", n_xs)
  } else {
    xs
  }
})


# choose tools
tool_ids <- tools %>% filter(tool_name %in% issues$title) %>% pull(tool_id)
tool_ids <- "scorpius"

for (tool_id in tool_ids) {
  print(tool_id)

  tool <- tools %>% filter(tool_id == !!tool_id) %>% extract_row_to_list(1)

  # get methods of this tool
  methods_oi <- methods %>% filter(tool_id == !!tool_id)

  # create texts
  hello_text <- map_chr(tool$authors, function(author) {
    if (!is.null(author$github)) {
      paste0("@", author$github)
    } else {
      author$given
    }
  }) %>% glue::glue_collapse(", ", last = " and ")

  code_text <- if(nrow(methods_oi) == 1) {
    glue::glue("[a docker container]({methods_oi$container_url})")
  } else {
    paste0(
      "docker containers",
      glue::glue_collapse(
        glue::glue("[[{seq_len(nrow(methods_oi))}]]({methods_oi$container_url})")
      )
    )
  }

  definition_text <- glue::glue("[definition.yml]({paste0(methods_oi$container_url, '/definition.yml') %>% first_non_na})")
  entrypoint_text <- glue::glue("[{basename(methods_oi$container_url %>% first_non_na)}]({methods_oi$container_url %>% first_non_na})")

  tool_information <- if(nrow(methods_oi) == 1) {
    ""
  } else {
    if (any(is.na(methods_oi$method_note))) {stop("Need a note when having multiple methods")}
    c(
      glue::glue("We created {nrow(methods_oi)} separate wrappers:"),
      glue::glue("- [**{methods_oi$name}**]({methods_oi$container_url}): {methods_oi$method_note}")
    ) %>% glue::collapse("\n")
  }

  qc_worksheet_url <- qc_ss$ws %>%
    filter(ws_title == tool_id) %>%
    pull(gid) %>%
    paste0(qc_ss$browser_url, "/edit#gid=", .)
  if (length(qc_worksheet_url) == 0) {stop("Implementation not found on google sheet")}
  qc_worksheet_text <- glue::glue("[the qc worksheet]({qc_worksheet_url})")

  blueprint <- glue::glue("
Hello {hello_text}

This issue is for discussing the wrapper for your trajectory inference method, {tool$tool_name}, which we wrapped for our benchmarking study ([10.1101/276907](https://doi.org/10.1101/276907)). In our [dynmethods](https://github.com/dynverse/dynmethods) framework, we collected some meta information about your method, and created a docker wrapper so that all methods can be easily run and compared. The code for this wrapper is located in {code_text}. The way this container is structured is described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html).

{tool_information}

We are creating this issue to ensure your method is being evaluated in the way it was designed for. The checklist below contains some important questions for you to have a look at.

- {tool$xs[[1]]} **Parameters**, defined in {definition_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#parameters))
  - Are all important parameters described in this file?
  - For each parameter, is the proposed default value reasonable?
  - For each parameter, is the proposed parameter space reasonable (e.g. lower and upper boundaries)?
  - Is the description of the parameters correct and up-to-date?
- {tool$xs[[2]]} **Input**, defined in {definition_text} and loaded in {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#input))
  - Is the correct type of expression requested (raw counts or normalised expression)?
  - Is all prior information (required or optional) requested?
  - Would some other type of prior information help the method?
- {tool$xs[[3]]} **Output**, defined in {definition_text} and saved in {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#output))
  - Is the output correctly processed towards the [common trajectory model](https://github.com/dynverse/dynwrap#dynwrap)? Would some other postprocessing method make more sense?
  - Is all relevant output saved (dimensionality reduction, clustering/grouping, pseudotime, ...)
- {tool$xs[[4]]} **Wrapper script**, see {entrypoint_text} ([more info](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html#doing-trajectory-inference))
  - This is a script that is executed upon starting the docker container. It will receive several input files as defined by `definition.yml`, and is expected to produce certain output files, also as defined by `definition.yml`.
  - Is the script a correct representation of the general workflow a user is expected to follow when they want to apply your method to their data?
- {tool$xs[[5]]} **Quality control**, see {qc_worksheet_text}
  - We also evaluated the tool of a method based on a large check list of good software software development practices.
  - Are the answers we wrote down for your method correct and up to date? Do you disagree with certain answers? (Feel free to leave a comment in the worksheet)
    - You can improve the QC score of your method by implementing the required changes and letting us know. *Do not gloss over this, as it is the easiest way to improve the overall ranking of your TI method in our study!*

The most convenient way for you to test and adapt the wrapper is to install [dyno](https://github.com/dynverse/dyno), download and modify [these files]({methods_oi$container_url[[1]]}), and run your method on a dataset of interest or one of our synthetic toy datasets. This is further described in [this vignette](https://dynverse.github.io/dynwrap/articles/create_ti_method_docker.html). Once finished, we prefer that you [fork the dynmethods repository](https://github.com/dynverse/dynmethods), make the necessary changes, and send us a pull request. Alternatively, you can also send us the files and we will make the necessary changes.

If you have any further questions or remarks, feel free to reply to this issue.

Kind regards,
{glue::glue_collapse(sample(c('@rcannood', '@zouter')), ' and ')}
")

  blueprint %>% clipr::write_clip()

  ##  ............................................................................
  ##  Patch github issue                                                      ####

  issue_number <- issues %>% filter(title == tool$tool_name) %>% pull(number)

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



