#' Overview heatmap plotting
#'
#' TODO: params need to be described
#'
#' @param data data
#' @param metric_ord metric_ord
#' @param method_ord method_ord
#' @param add_timestamp Whether or not to add a timestamp at the bottom
#' @param row_space The space between rows
#' @param row_bigspace The space between groups of rows
#' @param col_space The space between columns
#' @param col_bigspace The space between groups of columns
#'
#' @export
funky_heatmap <- function(
  data,
  metric_ord,
  method_ord,
  add_timestamp = TRUE,
  row_space = .1,
  row_bigspace = .5,
  col_space = .1,
  col_bigspace = .5
) {
  row_height <- 1 # must be fixed

  # SPREAD GEOM PARAMS
  metric_ord <- process_geom_params(metric_ord)

  # GENERATE METHOD POSITIONS
  method_pos <-
    method_ord %>%
    group_by(group) %>%
    mutate(group_i = row_number()) %>%
    ungroup() %>%
    mutate(
      method_i = row_number(),
      colour_background = group_i %% 2 == 1,
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      spacing = ifelse(do_spacing, row_bigspace, row_space),
      y = - (method_i * row_height + cumsum(spacing)),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )

  # GENERATE METHOD ANNOTATION
  method_annot <-
    method_pos %>%
    group_by(group) %>%
    summarise(
      ymin = min(ymin),
      ymax = max(ymax),
      y = (ymin + ymax) / 2,
      x = -1
    )

  # DETERMINE METRIC POSITIONS
  metric_pos <-
    metric_ord %>%
    mutate(
      xsep = c(0, ifelse(experiment[-1] != experiment[-n()] | category[-1] != category[-n()], col_bigspace, col_space)),
      xwidth = case_when(
        !is.na(width) ~ width,
        geom == "bar" ~ 4,
        geom == "text" ~ 2,
        geom == "traj" ~ 2,
        TRUE ~ 1
      ),
      overlay = !is.na(overlay) & overlay,
      show_label = is.na(show_label) | show_label,
      xsep = ifelse(overlay, c(0, -head(xwidth, -1)), xsep),
      xwidth = ifelse(overlay, -xsep, xwidth),
      xmax = cumsum(xwidth + xsep),
      xmin = xmax - xwidth,
      x = xmin + xwidth / 2
    )

  # GENERATE METRIC ANNOTATION
  metric_annot <-
    metric_pos %>%
    select(experiment, experiment_name, category_name, xmin, xmax) %>%
    mutate(row = row_number()) %>%
    gather(level, id, -row, -xmin, -xmax, -experiment) %>%
    group_by(level, experiment, id) %>%
    summarise(xmin = min(xmin), xmax = max(xmax)) %>%
    ungroup() %>%
    arrange(level != "experiment_name", xmin) %>%
    mutate(
      y = ifelse(level == "experiment_name", 4.5, 3.5),
      key = ifelse(level == "experiment_name", letters[row_number()], ""),
      x = (xmin + xmax) / 2
    )

  ####################################
  ###         PROCESS DATA         ###
  ####################################
  geom_data_processor <- function(geom_types, fun) {
    map_df(seq_len(nrow(metric_pos)), function(i) {
      if (!metric_pos$geom[[i]] %in% geom_types) {
        NULL
      } else {
        met_row <- metric_pos %>% slice(i)
        met <- crossing(met_row %>% select(-metric), data_frame(metric = unlist(met_row$metric)))
        dat <- data %>%
          filter(!placeholder) %>%
          inner_join(met, by = c("experiment", "category", "metric")) %>%
          left_join(method_pos, by = "method_id")
        fun(dat)
      }
    })
  }
  circle_data <- geom_data_processor("circle", function(dat) {
    dat %>% transmute(x0 = x, y0 = y, size = row_height / 2 * value, colour)
  })
  rect_data <- geom_data_processor("rect", function(dat) {
    dat %>% transmute(xmin, xmax, ymin, ymax, colour)
  })
  textbox_data <- geom_data_processor("textbox", function(dat) {
    dat %>% transmute(xmin, xmax, ymin, ymax, x, y, colour, label, colour_label = ifelse(value < .6, "white", "black"))
  })
  bar_data <- geom_data_processor("bar", function(dat) {
    # if ("inverse" %in% columns(dat)) {xmin = xmax - value * xmidth}
    dat %>% transmute(xmin = xmin, xmax = xmin + value * xwidth, ymin, ymax, colour)
  })
  text_data <- geom_data_processor("text", function(dat) {
    if (!"hjust" %in% colnames(dat)) {
      dat$hjust <- NA
    }
    if (!"vjust" %in% colnames(dat)) {
      dat$vjust <- NA
    }
    if (!"format" %in% colnames(dat)) {
      dat$format <- NA
    }

    dat %>% transmute(
      hjust = ifelse(is.na(hjust), .5, hjust),
      vjust = ifelse(is.na(vjust), .5, vjust),
      x = (1 - hjust) * xmin + hjust * xmax,
      y,
      colour = "black",
      label = pmap_chr(lst(format, label, value), function(format, label, value) {
        if (is.na(format) || format == "label") {
          label
        } else if (format == "value") {
          value
        } else if (format == "percentage") {
          if (value < .01) {
            "<1%"
          } else {
            paste0(ceiling(100 * value), "%")
          }
        }
      })
    )
  })
  pie_data <- geom_data_processor("pie", function(dat) {
    dat %>%
      select(-colour) %>%
      left_join(
        pie_colours,
        by = "metric"
      ) %>%
      group_by(method_id) %>%
      transmute(
        y0 = y,
        x0 = x,
        value = ifelse(is.finite(value), value, 0),
        pct = value / sum(value),
        rad = pct * 2 * pi,
        rad_end = cumsum(rad),
        rad_start = rad_end - rad,
        r0 = 0,
        r = row_height / 2,
        colour
      ) %>%
      filter(rad_end != rad_start) %>%
      ungroup()
  })
  barguides_data <- geom_data_processor("bar", function(dat) {
    crossing(
      data_frame(x = c(dat$xmin[[1]], dat$xmax[[1]])),
      method_annot %>% select(y = ymin, yend = ymax)
    )
  })
  trajd <- geom_data_processor("traj", function(dat) {
    dat %>%
      transmute(xmin, xmax, ymin, ymax, topinf = gsub("^gray_", "", label), colour = ifelse(grepl("^gray_", label), "lightgray", NA))
  })

  # Stamp
  if (add_timestamp) {
    stamp <- paste0("Generated on ", Sys.Date())
  }

  minimum_x <- min(metric_pos$xmin)
  maximum_x <- max(metric_pos$xmax)
  minimum_y <- min(method_pos$ymin)
  maximum_y <- max(method_pos$ymax)

  # CREATE LEGENDS
  # leg_circles <- map_df(names(exp_palettes), function(experiment) {
  #   data_frame(
  #     r = c(.05, .1, .2, .4, .6, .8, 1)/2,
  #     x = cumsum(c(0, (.05 + r[-1] + r[-length(r)]))),
  #     exp = experiment,
  #     pal = exp_palettes[[experiment]],
  #     col = scale_viridis_funs[[exp_palettes[[experiment]]]](x)
  #   )
  # })
  #
  # error_leg_df <- error_reasons %>%
  #   rename(fill = colour) %>%
  #   mutate(
  #     rad_start = seq(0, pi, length.out = 6) %>% head(-1),
  #     rad_end = seq(0, pi, length.out = 6) %>% tail(-1),
  #     rad = (rad_start + rad_end) / 2,
  #     colour = rep("black", length(rad)),
  #     lab_x = row_height * sin(rad),
  #     # lab_y = row_height * cos(rad),
  #     lab_y = seq(row_height * (cos(first(rad)) + .2), row_height * (cos(last(rad)) - .2), length.out = 5),
  #     hjust = rep(0, length(rad)),
  #     #vjust = seq(0, 1, length.out = length(rad)+2)[c(-1,-(length(rad)+2))]
  #     vjust = .5
  #   )


  ####################################
  ###         COMPOSE PLOT         ###
  ####################################
  g <- ggplot() +
    coord_equal(expand = FALSE) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    cowplot::theme_nothing()

  # ADD METRIC ANNOTATION
  df <- metric_annot %>% filter(id != "")
  g <- g +
    geom_segment(aes(x = xmin, xend = xmax, y = y, yend = y), df, size = 1) +
    geom_text(aes(x = x, y = y+.25, label = id), df, vjust = 0, hjust = 0.5, fontface = "bold") +
    geom_text(aes(x = xmin, y = y+.5, label = key), df %>% filter(key != ""), vjust = 0, hjust = 0, fontface = "bold", size = 5)

  # ADD METHOD ANNOTATION
  g <- g +
    geom_segment(aes(x = x, xend = x, y = ymin, yend = ymax), method_annot, size = 1) +
    geom_text(aes(x = x-.5, y = y, label = label_short(group)), method_annot, vjust = 0, hjust = .5, fontface = "bold", angle = 90)

  # ADD DATE
  if (add_timestamp) {
    g <- g + geom_text(aes(1, minimum_y-1, label = stamp), colour = "#cccccc", hjust = 0, vjust = 0)
  }

  # ADD SEPARATOR LINES
  df <- method_pos %>% filter(colour_background)
  if (nrow(df) > 0) {
    g <- g + geom_rect(aes(xmin = minimum_x-.25, xmax = maximum_x+.25, ymin = ymin - (row_space / 2), ymax = ymax + (row_space / 2)), df, fill = "#EEEEEE")
  }

  # ADD METRIC NAMES
  df <- metric_pos %>% filter(show_label)
  if (nrow(df) > 0) {
    g <- g +
      geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), metric_pos %>% filter(show_label), size = .5) +
      geom_text(aes(x = x, y = 0, label = label_long(name)), metric_pos %>% filter(show_label), angle = 30, vjust = 0, hjust = 0)
  }

  # ADD BAR GUIDES
  if (nrow(barguides_data) > 0) {
    g <- g + geom_segment(aes(x = x, xend = x, y = y, yend = yend), barguides_data, colour = "black", size = .5, linetype = "dashed")
  }

  # ADD BARS
  if (nrow(bar_data) > 0) {
    g <- g + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), bar_data, colour = "black", size = .25)
  }

  # ADD RECTANGLES
  if (nrow(rect_data) > 0) {
    g <- g + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), rect_data, colour = "black", size = .25)
  }

  # ADD TEXTBOXES
  if (nrow(textbox_data) > 0) {
    g <- g +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), textbox_data, colour = "black", size = .25) +
      geom_text(aes(x = x, y = y, label = label, colour = colour_label), textbox_data, hjust = .5, vjust = .5, size = 3)
  }

  # ADD CIRCLES
  if (nrow(circle_data) > 0) {
    g <- g + ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = colour, r = size), circle_data, size = .25)
  }

  # ADD STARS
  if (nrow(pie_data) > 0) {
    # plot pie pieces with pct ~= 1 as a circle
    g <- g +
      ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = colour), data = pie_data %>% filter(pct <= (1-1e-10)), size = .25) +
      ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = colour), data = pie_data %>% filter(pct > (1-1e-10)), size = .25)
  }

  # ADD TEXT
  if (nrow(text_data) > 0) {
    g <- g + geom_text(aes(x = x, y = y, label = label, colour = colour, hjust = hjust, vjust = vjust), data = text_data)
  }

  # ADD TRAJ TYPES
  if (nrow(trajd) > 0) {
    g <-
      plot_trajectory_types(
        plot = g,
        trajectory_types = trajd$topinf,
        xmins = trajd$xmin,
        xmaxs = trajd$xmax,
        ymins = trajd$ymin,
        ymaxs = trajd$ymax,
        node_colours = trajd$colour,
        edge_colours = trajd$colour,
        size = 1,
        geom = "circle",
        circ_size = .1
      )
  }

    # # RESERVE SPACE
    # expand_limits(x = c(-3, max(metric_pos$xmax)+3), y = c(legy_start - 4.3, 6.5)) +
    #
    # # LEGEND: BENCHMARK
    # geom_text(aes(header_xvals[["metric_prio"]], legy_start - 1, label = "Priors required"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
    # geom_text(aes(x = header_xvals[["metric_prio"]] + .8, y = legy_start - 2.3 + c(.8, 0, -.8), label = c("", "\u2715", "\u2716")), hjust = .5) +
    # geom_text(aes(x = header_xvals[["metric_prio"]] + 2, y = legy_start - 2.3 + c(.8, 0, -.8), label = c("None", "Soft", "Hard")), hjust = 0) +
    #
    # # LEGEND: BENCHMARK
    # geom_text(aes(header_xvals[["experiment_benchmark"]], legy_start - 1, label = "Benchmark score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
    # ggforce::geom_circle(aes(x0 = header_xvals[["experiment_benchmark"]] + .8 + x, y0 = legy_start - 2.3 + r, r = r, fill = col), size = .25, leg_circles %>% filter(exp == "benchmark")) +
    # geom_text(aes(x = header_xvals[["experiment_benchmark"]] + .8 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% filter(exp == "benchmark") %>% slice(c(1, n()))) +

    # # LEGEND: SCALING
    # geom_text(aes(header_xvals[["experiment_scalability"]], legy_start - 1, label = "Estimated time"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
    # ggforce::geom_circle(aes(x0 = header_xvals[["experiment_scalability"]] + .8 + x, y0 = legy_start - 2.3 + r, r = r, fill = col), size = .25, leg_circles %>% filter(exp == "scalability")) +
    # geom_text(aes(x = header_xvals[["experiment_scalability"]] + .8 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% filter(exp == "scalability") %>% slice(c(1, n()))) +

    # # LEGEND: QC
    # geom_text(aes(header_xvals[["experiment_qc"]], legy_start - 1, label = "QC score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
    # ggforce::geom_circle(aes(x0 = header_xvals[["experiment_qc"]] + .8 + x, y0 = legy_start - 2.3 + r, r = r, fill = col), size = .25, leg_circles %>% filter(exp == "qc")) +
    # geom_text(aes(x = header_xvals[["experiment_qc"]] + .8 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% filter(exp == "qc") %>% slice(c(1, n()))) +
    #
    # # LEGEND: PCT ERRORED
    # geom_text(aes(header_xvals[["metric_errr"]], legy_start - 1, label = "Error reason"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
    # ggforce::geom_arc_bar(aes(x0 = header_xvals[["metric_errr"]] + .5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = fill), size = .25, error_leg_df) +
    # ggforce::geom_arc_bar(aes(x0 = header_xvals[["metric_errr"]] + .5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = NA), size = .25, error_leg_df) +
    # geom_text(aes(x = header_xvals[["metric_errr"]] + .5 + lab_x + .5, y = legy_start - 2.5 + lab_y, label = label, vjust = vjust, hjust = hjust), error_leg_df) +
    # geom_segment(aes(x = header_xvals[["metric_errr"]] + .5, xend = header_xvals[["metric_errr"]] + .5, y = legy_start - 2.5, yend = legy_start - 2.5 + row_height*.75), data = data_frame(z = 1), size = .25) +


  g
}

process_geom_params <- function(metric_ord) {
  bind_cols(
    metric_ord %>% mutate(geom = gsub("\\(.*", "", geom)),
    map_df(metric_ord$geom, function(x) {
      if (grepl("\\(.*\\)", x)) {
        gsub("^[^\\(]*\\(", "list(", x) %>% parse(text = .) %>% eval() %>% as_data_frame()
      } else {
        tibble(a = 1)[,-1]
      }
    })
  )
}
