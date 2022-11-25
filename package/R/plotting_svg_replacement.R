#' Create replacers
#' Adds a replace_id column
#' @param to_replace Dataframe containing the data
#' @export
create_replacers <- function(to_replace) {
  to_replace %>% mutate(replace_id = map_chr(1-seq_len(n())/100, format, nsmall = 2))
}


#' Replace
#' Replaces the rectangles in a given svg with the svgs contained in the replacer
#' @param svg The svg to replace
#' @param replacer Replacer dataframe
#'
#' @importFrom xml2 xml_find_all xml_attr xml_attrs xml_new_root read_xml xml_set_attrs xml_add_child xml_remove
#' @importFrom utils type.convert
#'
#' @export
replace_svg <- function(svg, replacer) {
  requireNamespace("glue")

  if(!all(c("replace_id", "svg") %in% colnames(replacer))) stop("Replacer requires columns replace_id and svg")

  walk2(replacer$replace_id, replacer$svg, function(replace_id, sub_svg_str) {
    print(replace_id)

    # remove and extract rects
    rects <- svg %>% xml2::xml_find_all(".//d1:rect[contains(@style, 'fill: #ABCDEF; fill-opacity: ')]")

    # match rect
    matched_rects <- map_chr(rects, xml2::xml_attr, "style") %>%
      str_detect(stringr::str_glue("fill: #ABCDEF; fill-opacity: {replace_id};")) %>%
      which()

    print(matched_rects)

    if(length(matched_rects) > 1) warning("Multiple matched rectangles with replace_id")
    if(length(matched_rects) == 0) warning("No matched rectangles with replace_id")

    walk(matched_rects, function(matched_rect) {
      # find correct rect
      rectoi <- rects[[matched_rect]]

      attrs <- xml2::xml_attrs(rectoi) %>% map(utils::type.convert)

      # create sub_svg
      sub_svg <- xml2::xml_new_root(xml2::read_xml(sub_svg_str))

      # calculate scaling
      sub_svg_width <- xml2::xml_attr(sub_svg, "width") %>% gsub("(.*)pt", "\\1", .) %>% as.numeric()
      sub_svg_height <- xml2::xml_attr(sub_svg, "height") %>% gsub("(.*)pt", "\\1", .) %>% as.numeric()
      scale <- min(c(attrs$height/sub_svg_height, attrs$width/sub_svg_width))

      # create new group
      sub_g <- xml2::xml_new_root("g")

      # transform the group
      transform <- glue::glue_collapse(c(
        "translate({attrs$x}, {attrs$y})", # translate to box
        "translate({(attrs$width - sub_svg_width*scale)/2}, {(attrs$height - sub_svg_height*scale)/2})", # move to center
        "scale({scale})" # scale within box
      )) %>% stringr::str_glue()

      sub_g %>%
        xml2::xml_set_attrs(list(
          transform = transform
        ))

      # put sub_svg in group
      xml2::xml_add_child(sub_g, sub_svg)

      # add group to svg
      svg %>% xml2::xml_add_child(sub_g)

      # remove rectangle
      rectoi %>% xml2::xml_remove()
    })
  })

  svg
}
