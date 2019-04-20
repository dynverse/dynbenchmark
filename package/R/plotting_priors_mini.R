#' Creates a prior mini svg
#' @param method_priors Dataframe containing a prior_id and a prior_usage
#' @param svg The base svg of the priors
#' @export
#'
#' @importFrom xml2 read_xml xml_new_root xml_find_first xml_remove xml_add_child write_xml
generate_prior_mini <- function(method_priors, svg = xml2::read_xml(result_file("priors.svg", experiment_id = "priors"))) {
  svg2 <- xml2::xml_new_root(svg, .copy = T)

  map2(method_priors$prior_id, method_priors$prior_usage, function(prior_id, usage) {
    color <- dynbenchmark::prior_usages$color[dynbenchmark::prior_usages$prior_usage == usage]

    layer <- svg2 %>% xml2::xml_find_first(stringr::str_glue(".//svg:g[@inkscape:label='{prior_id}']")) %>% xml2::xml_remove()

    if(!is.na(usage) && usage != "no") {

      print(color)
      # only add layer back if needed
      layer %>%
        as.character() %>%
        gsub("stroke:#fb1e00", stringr::str_glue("stroke:{color}"), .) %>%
        gsub("fill:#fb1e00", stringr::str_glue("fill:{color}"), .) %>%
        {suppressWarnings(xml2::read_xml(.))} %>%
        xml2::xml_add_child(svg2, .)
    }
  })

  svg2 %>% xml2::write_xml("~/test.svg")

  svg2 %>% as.character()# %>% charToRaw() %>% base64enc::base64encode()
}
