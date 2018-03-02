library(tidyverse)
library(xml2)
library(dynalysis)

experiment("7-user_guidelines")

methods <- read_rds(derived_file("methods.rds", "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
trajtype_scores <- outputs_list$outputs_summtrajtype_totals %>% filter(task_source == "mean") %>% rename(method_id = method_short_name)

methods <-
  left_join(
    methods,
    read_rds(derived_file("implementation_qc_application_scores.rds", "4-method_characterisation")) %>%
      spread("application", "score"),
    "implementation_id"
  )


xml_attr_multiple <- function(x, attr, value) {
  walk2(
    x,
    value,
    ~(xml_attr(.x, attr) <- .y)
  )
}

xml_add_style <- function(x, value) {
  xml_attr_multiple(
    x,
    "style",
    paste0(xml_attr(x, "style"), ";", value)
  )
}


score_indicator <- function(score, cutoffs = c(0.9, 0.8, 0.65, 0.5)) {
  style = ""
  if (is.na(score)) {
    txt <- ""
  } else if(score > cutoffs[[1]]) {
    txt <- "++"
    style = "fill:green"
  } else if(score > cutoffs[[2]]) {
    txt <- "+"
    style = "fill:green"
  } else if (score > cutoffs[[3]]) {
    txt <- "±"
    style = "fill:orange"
  } else if(score > cutoffs[[4]]) {
    txt <- "-"
    style = "fill:red"
  } else if (score >= 0) {
    txt <- "--"
    style = "fill:red"
  } else {
    txt <- ""
  }
  lst(txt, style)
}


extract_top_methods <- function(leaf_id, n_top) {
  trajectory_type <- gsub("(.*)\\*", "\\1", leaf_id)
  if(leaf_id == "other_fixed") {
    # for now hardcode this other_fixed
    scores <- tibble(method_position = c(2), score=c(NA), method_id = c("elpigraph"))
  } else {
    # get trajectory types to score
    if (str_sub(leaf_id, -1) == "*") {
      trajectory_types_to_score <- trajectory_types$ancestors[[which(trajectory_types$id == trajectory_type)]]
    } else {
      trajectory_types_to_score <- trajectory_type
    }

    # find methods which can detect these trajectory types
    applicable_methods <- methods %>% filter(.data[[trajectory_type]]) %>% pull(method_id)

    # now get top methods and order them
    scores <- trajtype_scores %>%
      rename(score = harm_mean) %>%
      filter(method_id %in% applicable_methods) %>%
      filter(trajectory_type %in% trajectory_types_to_score) %>%
      group_by(method_id) %>%
      summarise(score = mean(score)) %>%
      top_n(n_top, score) %>%
      select(method_id, score) %>%
      arrange(-score) %>%
      mutate(method_position = row_number())

    # add ? if no high scoring methods
    # if(nrow(scores) == 1 | max(scores$score) < 0.5) {
    #   scores <- bind_rows(
    #     tibble(method_id = "?", score=1, method_position=1),
    #     scores %>% mutate(method_position = method_position+1)
    #   )
    # }

    # add not-evaluated methods if not enough methods in top
    if(nrow(scores) < n_top) {
      n_extra_methods <- n_top-nrow(scores)+1
      extra_methods <- applicable_methods %>% discard(~.%in%scores$method_id) %>% discard(~is.na(.)) %>% head(n_extra_methods)
      n_extra_methods <- length(n_extra_methods)
      scores <- bind_rows(
        scores,
        tibble(
          method_id = extra_methods,
          score=NA,
          method_position = seq_len(n_extra_methods) + nrow(scores)
        )
      )
    }
    n_top

    scores
  }

  scores <- scores %>% left_join(methods, "method_id")

  # add footnotes
  if (str_sub(leaf_id, -1) != "*" & trajectory_type %in% c("directed_cycle", "directed_linear", "bifurcation")) {
    scores$method_name[which(scores$topology_inference_type == "free")] <-
      paste0(scores$method_name[which(scores$topology_inference_type == "free")], " ∗")
  }

  # add dagger if low score
  scores <- scores %>% filter(score > 0.4 | is.na(score))
  # scores$method_name <- ifelse(scores$score < 0.4, paste0(scores$method_name, " ↘"), scores$method_name)

  # user friendly and performance indicators
  scores$user_friendly_indicator <- map(scores$user_friendly, score_indicator)
  scores$score_indicator <- map(scores$score - max(scores$score, na.rm=T), score_indicator, c(-0.0001, -0.05, -0.2, -1))

  # fix NA method name
  scores$method_name[which(is.na(scores$method_name))] <- "?"

  # add question mark if method not evaluated
  scores$method_name[which(!scores$evaluated)] <- paste0(scores$method_name[which(!scores$evaluated)], " §")

  # add style
  # scores$method_name_style <- paste0("font-size: ", c(4, 3, 2.5, 2)[seq_len(nrow(scores))])
  scores$method_name_style <- ""
  scores$method_name_style[which(!scores$evaluated)] <- paste0(scores$method_name_style[which(!scores$evaluated)], "; fill:#999999")

  scores
}

n_top <- 4

svg <- read_xml(figure_file("tree_raw.svg"))

leaf_nodeset <- svg %>% xml_find_all(".//svg:text[@class='methods']")
leaf_ids <- xml_attr(leaf_nodeset, "id")

leaf_methods <- map(leaf_ids, function(leaf_id) {
  print(leaf_id)

  scores <- extract_top_methods(leaf_id, n_top = n_top)
  n_methods <- nrow(scores)

  # method name
  method_node <- svg %>% xml_find_first(pritt(".//svg:text[@id='{leaf_id}']"))

  spans <- method_node %>% xml_children()

  xml_text(spans)[seq_len(n_methods)] <- scores$method_name
  xml_text(spans)[seq_len(length(spans)-n_methods)+n_methods] <- ""

  # vertical align
  line_diff <- first(diff(as.numeric(xml_attr(spans, "y"))))
  y_change <- line_diff/2 * (length(spans) - n_methods) # by how much should the y be changed
  xml_attr(method_node, "y") <- as.numeric(xml_attr(method_node, "y")) + y_change

  # user friendliness indicators
  user_friendly_node <- xml_add_sibling(method_node, method_node, .copy=T)

  spans <- user_friendly_node %>% xml_children()
  xml_attr(spans, "x") <- 272+1
  xml_attr(user_friendly_node, "x") <- 272+1

  xml_text(spans)[seq_len(n_methods)] <- map_chr(scores$user_friendly_indicator,"txt")
  xml_add_style(spans,map_chr(scores$user_friendly_indicator, "style"))
  xml_text(spans)[seq_len(length(spans)-n_methods)+n_methods] <- ""

  # performance indicators
  performance_node <- xml_add_sibling(method_node, method_node, .copy=T)

  spans <- performance_node %>% xml_children()
  xml_attr(performance_node, "x") <- 280+1
  xml_attr(spans, "x") <- 280+1

  xml_text(spans)[seq_len(n_methods)] <- map_chr(scores$score_indicator,"txt")
  xml_add_style(spans,map_chr(scores$score_indicator, "style"))
  xml_text(spans)[seq_len(length(spans)-n_methods)+n_methods] <- ""

  # method_size
  spans <- method_node %>% xml_children()

  xml_add_style(
    spans,
    scores$method_name_style
  )
})

date <- xml_child(xml_find_first(svg, ".//svg:text[@id='date']"))
xml_text(date) <- paste0("Generated at ", as.character(Sys.Date()))

svg %>% write_xml(figure_file("tree.svg"))

system(pritt("inkscape {figure_file('tree.svg')} --export-png {figure_file('tree.png')} -d 300"))
