#' Overview heatmap plotting
#'
#' TODO: params need to be described
#'
#' @param data data
#' @param metric_ord metric_ord
#' @param method_ord method_ord
#' @param add_timestamp Whether or not to add a timestamp at the bottom
#'
#' @export
funky_heatmap <- function(
  data,
  column_info,
  column_groups,
  row_info,
  row_groups,
  palettes,
  scale_column = TRUE,
  add_timestamp = TRUE
) {
  # no point in making these parameters
  row_height <- 1
  row_space <- .1
  row_bigspace <- .5
  col_space <- .1
  col_bigspace <- .5

  # SPREAD GEOM PARAMS
  column_info <- process_geom_params(column_info)

  # GENERATE ROW POSITIONS
  row_groups$group_spacing <- c(0, ifelse(diff(as.integer(factor(row_groups$group))) != 0, row_bigspace, row_space))

  row_pos <-
    row_info %>%
    group_by(group) %>%
    mutate(group_i = row_number()) %>%
    ungroup() %>%
    left_join(row_groups %>% select(group, group_spacing), by = "group") %>%
    mutate(
      row_i = row_number(),
      colour_background = group_i %% 2 == 1,
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      ysep = ifelse(do_spacing, group_spacing, row_space),
      y = - (row_i * row_height + cumsum(ysep)),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )

  column_groups$group_spacing <- c(0, ifelse(diff(as.integer(factor(column_groups$group))) != 0, col_bigspace, col_space))

  # DETERMINE COLUMN POSITIONS
  if (!"overlay" %in% colnames(column_info)) {
    column_info$overlay <- FALSE
  }
  column_pos <-
    column_info %>%
    left_join(column_groups %>% select(group, group_spacing), by = "group") %>%
    mutate(
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      xsep = ifelse(do_spacing, group_spacing, col_space),
      xwidth = case_when(
        !is.na(width) ~ width,
        geom == "bar" ~ 4,
        geom == "text" ~ 2,
        geom == "traj" ~ 2,
        TRUE ~ 1
      ),
      overlay = !is.na(overlay) & overlay,
      xsep = ifelse(overlay, c(0, -head(xwidth, -1)), xsep),
      xwidth = ifelse(overlay, -xsep, xwidth),
      xmax = cumsum(xwidth + xsep),
      xmin = xmax - xwidth,
      x = xmin + xwidth / 2
    )

  # GENERATE ROW ANNOTATION
  row_annotation <-
    row_groups %>%
    select(-group_spacing) %>%
    gather(level, name, -group) %>%
    left_join(row_pos %>% select(group, ymin, ymax), by = "group") %>%
    group_by(level, name) %>%
    summarise(
      ymin = min(ymin),
      ymax = max(ymax),
      y = (ymin + ymax) / 2
    ) %>%
    ungroup() %>%
    mutate(
      levelmatch = match(level, colnames(row_groups)),
      xmin = levelmatch - max(levelmatch) - 2,
      xmax = xmin + 1,
      x = (xmin + xmax) / 2
    )

  # GENERATE COLUMN ANNOTATION
  column_annotation <-
    column_groups %>%
    select(-group_spacing) %>%
    gather(level, name, -group) %>%
    left_join(column_pos %>% select(group, xmin, xmax), by = "group") %>%
    group_by(level, name) %>%
    summarise(
      xmin = min(xmin),
      xmax = max(xmax),
      x = (xmin + xmax) / 2
    ) %>%
    ungroup() %>%
    mutate(
      levelmatch = match(level, colnames(column_groups)),
      ymin = max(levelmatch) - levelmatch + 3,
      ymax = ymin + 1,
      y = (ymin + ymax) / 2
    )

  # FIGURE OUT LEGENDS
  if (!"legend" %in% colnames(column_info)) column_info$legend <- NA
  legends_to_plot <-
    column_info %>%
    mutate(legend = is.na(legend) | legend) %>%
    filter(!is.na(palette), legend) %>%
    group_by(palette) %>%
    arrange(desc(legend)) %>%
    slice(1) %>%
    ungroup() %>%
    select(palette, group, geom)

  # FIGURE OUT PALETTES
  palette_assignment <-
    column_info %>%
    filter(!is.na(palette)) %>%
    select(id, palette)

  palette_list <- palettes %>% deframe()

  ####################################
  ###         PROCESS DATA         ###
  ####################################
  geom_data_processor <- function(geom_types, fun) {
    column_sel <-
      column_pos %>%
      filter(geom %in% geom_types) %>%
      select(-group, -name, -group_spacing, -do_spacing) %>%
      rename(column_id = id)

    if (nrow(column_sel) == 0) {
      return(tibble(a = 1) %>% slice(integer()))
    }

    row_sel <-
      row_pos %>%
      select(-group, -group_i, -group_spacing, -row_i, -colour_background, -do_spacing) %>%
      rename(row_id = id)

    data_sel <-
      data %>%
      select(row_id = id, !!column_sel$column_id) %>%
      gather(column_id, value, -row_id)

    dat <-
      data_sel %>%
      left_join(column_sel, by = "column_id") %>%
      left_join(row_sel, by = "row_id")

    # apply function
    dat <- fun(dat)

    # scale data, if need be
    if (scale_column && is.numeric(dat$value)) {
      dat <-
        dat %>%
        group_by(column_id) %>%
        mutate(value = dynutils::scale_minmax(value)) %>%
        ungroup()
    }

    # determine colours
    if (any(!is.na(column_sel$palette))) {
      if (is.character(dat$value)) {
        dat <- dat %>% mutate(col_value = ifelse(is.na(palette), NA, value))
      } else if (is.numeric(dat$value)) {
        dat <- dat %>% mutate(col_value = ifelse(is.na(palette), NA, round(value * 100) + 1))
      } else {
        dat$col_value <- NA
      }


      dat <-
        dat %>%
        group_by(palette) %>%
        mutate(
          colour = {
            if (!is.na(palette[[1]])) {
              case_when(
                is.na(palette) ~ as.character(NA),
                is.na(col_value) ~ "#444444FF",
                palette %in% names(palette_list) ~ palette_list[[palette[[1]]]][col_value],
                TRUE ~ as.character(NA)
              )
            } else {
              as.character(NA)
            }
          }
        ) %>%
        ungroup()
    }

    dat
  }
  circle_data <- geom_data_processor("circle", function(dat) {
    dat %>% mutate(x0 = x, y0 = y, size = row_height / 2 * value)
  })
  rect_data <- geom_data_processor("rect", identity)
  textbox_data <- geom_data_processor("textbox", function(dat) {
    dat %>% mutate(
      label = map_chr(value, ~ .$label),
      value = map_dbl(value, ~ .$value),
      colour_label = ifelse(value < .6, "white", "black")
    )
  })
  bar_data <- geom_data_processor("bar", function(dat) {
    if (!"align_left" %in% colnames(dat)) {
      dat$align_left <- TRUE
    }
    dat %>% mutate(
      xmin = ifelse(align_left, xmin, xmax - value * xmidth),
      xmax = ifelse(align_left, xmin + value * xwidth, xmax)
    )
  })
  text_data <- geom_data_processor("text", function(dat) {
    if (!"hjust" %in% colnames(dat)) {
      dat$hjust <- NA
    }
    if (!"vjust" %in% colnames(dat)) {
      dat$vjust <- NA
    }

    dat %>% mutate(
      hjust = ifelse(is.na(hjust), .5, hjust),
      vjust = ifelse(is.na(vjust), .5, vjust),
      x = (1 - hjust) * xmin + hjust * xmax,
      colour = "black", # colour can be overridden of needed
      label = value
    )
  })
  pie_data <- geom_data_processor("pie", function(dat) {
    dat <-
      inner_join(
        dat %>% select(-value) %>% mutate(iii = row_number()),
        dat %>% select(value) %>% mutate(iii = row_number()) %>%
          dynutils::mapdf_dfr(function(l) {
            enframe(l$value) %>% mutate(iii = l$iii)
          }),
        by = "iii"
      ) %>%
      select(-iii)

    dat %>%
      group_by(row_id, column_id) %>%
      mutate(
        y0 = y,
        x0 = x,
        pct = ifelse(is.finite(value), value, 0),
        pct = pct / sum(pct),
        rad = pct * 2 * pi,
        rad_end = cumsum(rad),
        rad_start = rad_end - rad,
        r0 = 0,
        r = row_height / 2,
        value = name
      ) %>%
      filter(rad_end != rad_start) %>%
      ungroup()
  })
  barguides_data <- geom_data_processor("bar", function(dat) {
    crossing(
      dat %>% group_by(column_id) %>% slice(1) %>% ungroup() %>% select(xmin, xmax) %>% gather(col, x) %>% select(-col),
      row_pos %>% select(y = ymin, yend = ymax)
    ) %>%
      mutate(palette = NA, value = NA)
  })
  trajd <- geom_data_processor("traj", function(dat) {
    dat %>%
      mutate(topinf = gsub("^gray_", "", value), colour = ifelse(grepl("^gray_", value), "lightgray", NA))
  })

  minimum_x <- min(column_pos$xmin)
  maximum_x <- max(column_pos$xmax)
  minimum_y <- min(row_pos$ymin)
  maximum_y <- max(row_pos$ymax)

  # Stamp
  if (add_timestamp) {
    stamp <- paste0("Generated on ", Sys.Date())
    minimum_y <- minimum_y - 1
    stamp_y <- minimum_y
  }

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

  # ADD SEPARATOR LINES
  df <- row_pos %>% filter(colour_background)
  if (nrow(df) > 0) {
    g <- g + geom_rect(aes(xmin = minimum_x-.25, xmax = maximum_x+.25, ymin = ymin - (row_space / 2), ymax = ymax + (row_space / 2)), df, fill = "#EEEEEE")
  }

  # ADD COLUMN ANNOTATION
  df <- column_annotation %>% filter(name != "")
  g <- g +
    geom_segment(aes(x = xmin, xend = xmax, y = ymin, yend = ymin), df, size = 1) +
    geom_text(aes(x = x, y = ymin, label = name), df, vjust = 0, hjust = 0.5, fontface = "bold", nudge_y = .1) +
    expand_limits(y = max(df$ymax))
    # geom_text(aes(x = xmin, y = y+.5, label = key), df %>% filter(key != ""), vjust = 0, hjust = 0, fontface = "bold", size = 5)

  # ADD ROW ANNOTATION
  df <- row_annotation %>% filter(name != "")
  g <- g +
    geom_segment(aes(x = xmax, xend = xmax, y = ymin, yend = ymax), df, size = 1) +
    geom_text(aes(x = xmax, y = y, label = name), df, vjust = 0, hjust = 0.5, fontface = "bold", angle = 90, nudge_x = -.1) +
    expand_limits(x = min(df$xmin))

  # ADD DATE
  if (add_timestamp) {
    g <- g + geom_text(aes(1, stamp_y, label = stamp), colour = "#cccccc", hjust = 0, vjust = 0)
  }

  # ADD METRIC NAMES
  df <- column_pos %>% filter(name != "")
  if (nrow(df) > 0) {
    g <- g +
      geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), df, size = .5) +
      geom_text(aes(x = x, y = 0, label = label_long(name)), df, angle = 30, vjust = 0, hjust = 0)
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
