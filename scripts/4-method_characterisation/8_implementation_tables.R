library(googlesheets)
library(tidyverse)
library(dynalysis)

experiment("4-method_characterisation")

implementations <- read_rds(derived_file("implementations.rds"))

implementations <- implementations %>%
  filter(contains_ti) %>%
  arrange(date)

# process priors
prdf <- implementations %>%
  select(implementation_id, !!priors$prior_id) %>%
  gather(prior_id, value, -implementation_id) %>%
  left_join(priors, by = "prior_id")
pr_required <- prdf %>%
  filter(!is.na(value)) %>%
  mutate(required = grepl("required", value)) %>%
  group_by(implementation_id) %>%
  summarise(prior_required = ifelse(any(required), paste(prior_name[required], collapse = ", "), "None"))
pr_optional <- prdf %>%
  filter(!is.na(value)) %>%
  mutate(optional = grepl("can_use", value)) %>%
  group_by(implementation_id) %>%
  summarise(prior_optional = ifelse(any(optional), paste(prior_name[optional], collapse = ", "), "None"))

implementations <- implementations %>%
  left_join(pr_required, by = "implementation_id") %>%
  left_join(pr_optional, by = "implementation_id")

# add non inclusion footnotes
implementations$non_inclusion_reasons_footnotes <- implementations$non_inclusion_reasons_split %>%
  map(function(reasons) {
    slice(non_inclusion_reasons, match(reasons, id))$footnote %>% sort()
  })

##  ............................................................................
##  Paper table                                                             ####
superscript <- c(latex = function(x) pritt("\\textsuperscript{{{x}}}"), html = function(x) pritt("<sup>{x}</sup>"))

citation_format <- c(
  latex = function(references) {
    if(is.na(references)) {
      ""
    } else {
      references %>%
        gsub("@", "", .) %>%
        str_split("[, \\n]+") %>%
        first() %>%
        glue::collapse(",") %>%
        paste0("\\cite{", ., "}")
    }
  },
  html = function(references) {
    if(is.na(references)) {
      ""
    } else {
      references %>%
        str_split("[, \\n]+") %>%
        first() %>%
        paste0("@", .) %>%
        glue::collapse(";") %>%
        {pritt("[{.}]")}
    }
  }
)

imp_table <- map(c("latex", "html"), function(format) {
  implementations_table <-
    implementations %>%
    filter(type == "algorithm") %>%
    arrange(date) %>%
    mutate(
      evaluated = ifelse(evaluated, "Yes", map_chr(non_inclusion_reasons_footnotes, ~paste0("No" , superscript[[format]](paste0(., collapse = " "))))),
      evaluated = kableExtra::cell_spec(
        evaluated,
        format,
        background = ifelse(evaluated == "Yes", "#666666", "white"),
        color = ifelse(evaluated == "Yes", "white", "black"),
        escape = FALSE
      ),
      date = strftime(date, "%d/%m/%Y"),
      maximal_trajectory_type =
        kableExtra::cell_spec(
          label_simple_trajectory_types(maximal_trajectory_type),
          format,
          background =
            toupper(set_names(trajectory_types$colour, trajectory_types$id)[maximal_trajectory_type]) %>%
            map(~replace(., is.na(.), "#666666")),
          color = "#FFFFFF"
        ),
      implementation_name = kableExtra::cell_spec(
        implementation_name,
        format,
        link = code_location
      ),
      reference = kableExtra::cell_spec(
        map_chr(bibtex, citation_format[[format]]),
        format,
        escape = FALSE
      ),
      fixes_topology = kableExtra::cell_spec(
        ifelse(is.na(topology_inference_type), "TBD", label_long(topology_inference_type)),
        format,
        color = ifelse(is.na(topology_inference_type), "gray", setNames(topinf_types$colour, topinf_types$name)[topology_inference_type]),
        escape = FALSE
      ),
      prior_required = kableExtra::cell_spec(
        ifelse(is.na(prior_required), "TBD", prior_required),
        format,
        color = ifelse(is.na(prior_required), "gray", "black"),
        escape = FALSE
      ),
      prior_optional = kableExtra::cell_spec(
        ifelse(is.na(prior_optional), "TBD", prior_optional),
        format,
        color = ifelse(is.na(prior_optional), "gray", "black"),
        escape = FALSE
      )
    ) %>%
    select(method = implementation_name, date, maximal_trajectory_type,evaluated, reference) %>%
    rename_all(label_long)

  # force newline most complex trajectory type -_-
  if(format == "latex") {
    implementations_table <- implementations_table %>%
      rename_at(label_long("maximal_trajectory_type"), ~paste0("\\pbox{20cm}{", label_wrap(., 20, "\\\\[-0.5em]"), "}")) %>%
      mutate_all(~gsub("\\#", "\\\\#", .)) # craze regular expressions :p
  }

  table <- implementations_table %>%
    knitr::kable(format, escape = F) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover","condensed"), font_size = ifelse(format == "latex", 7, 12))
  table
}) %>% set_names(c("latex", "html"))
imp_table
write_rds(imp_table, figure_file("implementations_table.rds"))



##  ............................................................................
##  Google sheet                                                            ####
sheet <- gs_url("https://docs.google.com/spreadsheets/d/1xeRtfa4NZR-FBZOosPv6nY-alfoGE1ZCEP2ceQg6GH0/edit#gid = 0")

label_tbd <- function(x) ifelse(is.na(x), "TBD", label_long(x))

direct <- function(x) x
url <- function(url, text = url) ifelse(is.na(url), "", pritt(" = HYPERLINK('{url}', '{text}')"))
cite <- function(doi) ifelse(is.na(doi), "", url(pritt("https://doi.org/{doi}"), doi))
date <- function(x) x
columns <- tribble(
  ~sheet_id, ~local_id, ~processor,
  "Name", "implementation_name", direct,
  "Most complex trajectory type", "maximal_trajectory_type", label_simple_trajectory_types,
  "Earliest publishing date", "date", date,
  "Fixes topology", "topology_inference_type", label_tbd,
  "Priors required", "prior_required", label_tbd,
  "Priors optional", "prior_optional", label_tbd,
  "Evaluated", "evaluated", label_long,
  "Reference" , "doi", cite,
  "Code/package", "code_location", url
)

# the order in the spreadsheet is the reference, as then conditional formatting is copied over :)
sheet_columns <- gs_read(sheet) %>%
  colnames() %>%
  tibble(sheet_id = .) %>%
  left_join(columns, "sheet_id")

newsheet <- sheet_columns %>% pmap(function(local_id, processor, ...) {
  implementations[[local_id]] %>%
    processor() %>% str_replace_all("'", "\"")
}) %>% set_names(sheet_columns$sheet_id) %>% as_tibble()

gs_edit_cells(sheet, "Methods", newsheet)
