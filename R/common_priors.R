#' The priors
#' @export
priors <- tribble(
  ~prior_id,              ~prior_name,
  "grouping_assignment",  "Groupings",
  "marker_feature_ids",   "Marker genes",
  "end_cells",            "End cell(s)",
  "n_branches",           "Num. branches",
  "start_cells",          "Start cell(s)",
  "n_end_states",         "Num. end states",
  "time",                 "Time",
  "timecourse",           "Timecourse"
)

#' The prior usages
#' @export
prior_usages <- tribble(
  ~prior_usage, ~color,
  "can_use", "#0074D9",
  "can_root", "#39CCCC",
  "no", "#EEEEEE",
  "?", "#AAAAAA",
  "required", "#FF4136",
  "required_default", "#FF4136"
)

#' Creates a prior mini svg
#' @param method_priors Dataframe containing a prior_id and a prior_usage
#' @param svg The base svg of the priors
#' @export
generate_prior_mini <- function(method_priors, svg = xml2::read_xml(figure_file("priors.svg", experiment_id = "priors"))) {
  svg2 <- xml2::xml_new_root(svg, .copy=T)

  map2(method_priors$prior_id, method_priors$prior_usage, function(prior_id, usage) {
    color <- prior_usages$color[prior_usages$prior_usage == usage]

    layer <- svg2 %>% xml2::xml_find_first(pritt(".//svg:g[@inkscape:label='{prior_id}']")) %>% xml2::xml_remove()

    if(!is.na(usage) && usage != "no") {

      print(color)
      # only add layer back if needed
      layer %>%
        as.character() %>%
        gsub("stroke:#fb1e00", pritt("stroke:{color}"), .) %>%
        gsub("fill:#fb1e00", pritt("fill:{color}"), .) %>%
        {suppressWarnings(xml2::read_xml(.))} %>%
        xml2::xml_add_child(svg2, .)
    }
  })

  svg2 %>% xml2::write_xml("~/test.svg")

  svg2 %>% as.character()# %>% charToRaw() %>% base64enc::base64encode()
}
