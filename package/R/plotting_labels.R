#' Short labelling function
#' @param x The text
#' @param width The width of the label
#' @export
label_short <- function(x, width = 10) {
  tibble(id = as.character(x)) %>%
    left_join(dynbenchmark::labels, "id") %>%
    mutate(short = ifelse(is.na(short), label_capitalise(id), short)) %>%
    mutate(short = label_wrap(short, width = width)) %>%
    pull(short) %>%
    set_names(names(x))
}

#' Text wrapping
#'
#' @param x a character vector.
#' @param width a positive integer giving the target column for wrapping lines in the output.
#' @param collapse an optional character string to separate the different lines.
#'
#' @export
label_wrap <- function(x, width = 10, collapse = "\n") {
  strwrap(x, width, simplify = FALSE) %>% map_chr(paste0, collapse = collapse)
}

#' Long labelling function
#' @param x The text
#' @export
label_long <- function(x) {
  tibble(id = as.character(x)) %>%
    left_join(dynbenchmark::labels, "id") %>%
    mutate(
      long = ifelse(
        is.na(long),
        id %>% label_n %>% label_perc %>% label_capitalise,
        long
      )
    ) %>%
    pull(long) %>%
    set_names(names(x))
}
label_n <- function(x) {
  x %>% gsub("^n_", "# ", .)
}
label_perc <- function(x) {
  x %>% gsub("_perc$", "_%", .)
}

#' Capitalise label
#' @param x The text
#' @export
label_capitalise <- function(x) {
  capitalise <- function(string) {
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
    string
  }

  x %>% str_replace_all("_", " ") %>% capitalise()
}


#' Labeller for facets
#' @param label_func Which function to use for facet labelling
#' @param ... Passed to mutate_all
#' @export
label_facet <- function(label_func = label_long, ...) {
  function(df) {
    mutate_all(df, label_func, ...)
  }
}

#' Labels only the extrema (and zero)
#' @param x The values
#' @export
label_extrema <- function(x) {
  ifelse(x %in% c(0, min(x), max(x)), x, "")
}


#' Label p_value
#' @param p_values P values
#' @param cutoffs Cutoffs for number of asterisks
#'
#' @export
label_pvalue <- function(p_values, cutoffs = c(0.1, 0.01, 1e-5)) {
  requireNamespace("glue")

  breaks <- c(Inf, cutoffs, -Inf)
  labels <- rev(c("NS", map_chr(seq_len(length(cutoffs)), ~glue::glue_collapse(rep("*", .)))))
  p_values %>% cut(breaks, labels) %>% as.character()
}




#' Label the metrics
#' @param metric_id metric id
#' @param parse Whether to parse the label into an expression
#' @param format The format of the label to return, can be plotmath, latex, long_name or metric_id
#'
#' @export
label_metric <- function(metric_id, format = get_default_metric_format(), parse = FALSE) {
  if (length(metric_id) > 1) {stop("Needs only one metric_id")}

  if (metric_id %in% dyneval::metrics$metric_id) {
    if (!format %in% colnames(dyneval::metrics)) stop(format, " not found in dyneval::metrics")
    label <- dyneval::metrics[[format]][match(metric_id, dyneval::metrics$metric_id)]
  } else {
    label <- label_long(metric_id)
  }

  if (parse) {
    label <- gsub(" ", " ~~ ", label) # space -> ~~
    label <- parse(text = label)
  }

  if (format == "latex") {
    label <- paste0("$", label, "$")
  }

  label
}

#' @rdname label_metric
#' @param metric_ids The metric ids
#' @param ... Extra parameters for label_metric
#' @export
label_metrics <- function(metric_ids, ...) {
  map(metric_ids, label_metric, ...)
}


get_default_metric_format <- function() {
  case_when(
    identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "latex")  ~ "latex",
    identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "html")  ~ "html",
    identical(knitr::opts_knit$get("out.format"), "markdown") ~ "html",
    TRUE ~ "plotmath"
  )
}

#' Get the limits of a metric
#' @param metric_id metric id
#' @export
limits_metric <- function(metric_id) {
  if (metric_id %in% dyneval::metrics$metric_id) {
    extract_row_to_list(dyneval::metrics, which(metric_id == !!metric_id))[c("worst", "perfect")] %>% unlist()
  } else {
    c(worst = 0, perfect = 1)
  }
}



#' Label time
#'
#' @param time Time
#'
#' @export
label_time <- function(time) {
  case_when(
    time < 1 ~ "<1s",
    time < 60 ~ paste0(floor(time), "s"),
    time < 3600 ~ paste0(floor(time / 60), "m"),
    time < 3600 * 24 ~ paste0(floor(time / 3600), "h"),
    time < 3600 * 24 * 7 ~ paste0(floor(time / 3600 / 24), "d"),
    TRUE ~ ">7d"
  )
}

#' Label memory
#' @param x Memory in bytes
#'
#' @export
label_memory <- function(x) {
  map_chr(x, function(x) {
    if (is.na(x)) {
      NA
    } else if (x < 10^6) {
      paste0(round(x/10^3), "kB")
    } else if (x < 10^9) {
      paste0(round(x/10^6), "MB")
    } else if (x < 10^12) {
      paste0(round(x/10^9), "GB")
    } else {
      paste0(round(x/10^12), "TB")
    }
  })
}


#' Label thousands
#' @param x Numeric value
#'
#' @export
label_thousands <- function(x) {
  map_chr(x, function(x) {
    if (is.na(x)) {
      NA
    } else if (x < 10^3) {
      as.character(round(x))
    } else if (x < 10^6) {
      paste0(round(x/10^3), "k")
    } else {
      paste0(round(x/10^6), "M")
    }
  })
}

#' tag the first plot of an assemble
#'
#' @param x A ggassemble
#' @param tag The tag
#'
#' @export
tag_first <- function(x, tag) {
  y <- x$assemble$plots[[1]]

  if ("ggassemble" %in% class(y)) {
    x$assemble$plots[[1]] <- tag_first(x$assemble$plots[[1]], tag = tag)
  } else {
    x$assemble$plots[[1]] <- x$assemble$plots[[1]] + labs(tag = tag)
  }
  x
}




#' Collapse a vector with commas and "ands"
#'
#' @param x A vector
#' @export
label_vector <- function(x) {
  if (length(x) == 0) {
    ""
  } else if (length(x) == 1) {
    x
  } else {
    glue::glue_collapse(x, sep = ", ", last = " and ")
  }
}



#' Label a method
#'
#' @param method_ids Vector of method ids
#' @export
label_method <- function(method_ids) {
  methods <- load_methods()
  methods$name[match(method_ids, methods$id)]
}



