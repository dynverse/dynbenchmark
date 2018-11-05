#' Overview heatmap plotting
#'
#' TODO: params need to be described
#'
#' @param data data
#' @param column_info A data frame describing the columns of `data`. This data frame should contain the following columns:
#'   * `id` (`character`): The corresponding column name in `data`.
#'   * `name` (`character`): A label for the column. If `NA` or `""`, no label will be plotted.
#'   * `group` (`character`): The group of the columns. If all are `NA`, the columns will not be split up into groups.
#'   * `geom` (`character`): The geom of the column. Must be one of: `circle`, `rect`, `bar`, `pie`, or `text`.
#'   * `palette` (`character`): Which palette to colour the geom by.
#'   * `options` (`list`): Column specific options. The content of the list will depend on the geom. Options are:
#'     - `width`: Custom width for this column (default: 1).
#'     - `overlay`: Whether to overlay this column over the previous column. If so, the width of that column will be inherited.
#'     - `legend`: Whether or not to add a legend for this column.
#'     - `hjust`, `vjust`, `size`: see [ggplot2::geom_text].
#'     - `label` (`geom = "text"`): Which column to use as a label.
#'     - `hjust` (`geom = "bar"`): Horizontal alignment of the bar, must be between \[0,1\].
#' @param row_info A data frame describing the rows of `data`. This data should contain the following columns:`
#'   * `id` (`character`): The corresponding row name in `data`.
#'   * `group` (`character`): The group of the row. If all are `NA`, the rows will not be split up into groups.
#' @param palettes e
#' @param column_groups b
#' @param row_groups d
#' @param scale_column f
#' @param add_timestamp Whether or not to add a timestamp at the bottom
#'
#' @importFrom ggforce geom_arc_bar geom_circle
#' @importFrom cowplot theme_nothing
#'
#' @export
funky_heatmap <- function(
  data,
  column_info,
  row_info,
  palettes,
  column_groups = NULL,
  row_groups = NULL,
  scale_column = TRUE,
  add_timestamp = TRUE
) {
  # no point in making these into parameters
  row_height <- 1
  row_space <- .1
  row_bigspace <- .5
  col_width <- 1
  col_space <- .1
  col_bigspace <- .5

  row_annot_offset <- 1
  col_annot_offset <- 3

  # SPREAD GEOM PARAMS
  column_info <- process_geom_params(column_info)

  # GENERATE ROW POSITIONS
  if (!"group" %in% colnames(row_info) || all(is.na(row_info$group))) {
    row_info$group <- ""
    row_groups <- tibble(group = "")
    plot_row_annotation <- FALSE
  } else {
    plot_row_annotation <- TRUE
  }

  row_pos <-
    row_info %>%
    group_by(group) %>%
    mutate(group_i = row_number()) %>%
    ungroup() %>%
    mutate(
      row_i = row_number(),
      colour_background = group_i %% 2 == 1,
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      ysep = ifelse(do_spacing, row_bigspace, row_space),
      y = - (row_i * row_height + cumsum(ysep)),
      ymin = y - row_height / 2,
      ymax = y + row_height / 2
    )

  # DETERMINE COLUMN POSITIONS
  if (!"group" %in% colnames(column_info) || all(is.na(column_info$group))) {
    column_info$group <- ""
    plot_column_annotation <- FALSE
  } else {
    plot_column_annotation <- TRUE
  }

  if (!"opt_width" %in% colnames(column_info)) column_info$opt_width <- NA
  if (!"opt_overlay" %in% colnames(column_info)) column_info$opt_overlay <- NA

  column_pos <-
    column_info %>%
    mutate(
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      xsep = ifelse(do_spacing, col_bigspace, col_space),
      xwidth = ifelse(!is.na(opt_width), opt_width, col_width),
      opt_overlay = !is.na(opt_overlay) & opt_overlay,
      xsep = ifelse(opt_overlay, c(0, -head(xwidth, -1)), xsep),
      xwidth = ifelse(opt_overlay, -xsep, xwidth),
      xmax = cumsum(xwidth + xsep),
      xmin = xmax - xwidth,
      x = xmin + xwidth / 2
    )

  # FIGURE OUT PALETTES
  palette_assignment <-
    column_info %>%
    filter(!is.na(palette)) %>%
    select(id, palette)

  palette_list <- palettes %>% deframe()

  ####################################
  ###         PROCESS DATA         ###
  ####################################
  geom_data_processor <- make_geom_data_processor(data, column_pos, row_pos, scale_column, palette_list)

  # gather segment data
  segment_data <- NULL # is this a use case?

  # gather circle data
  circle_data <- geom_data_processor("circle", function(dat) {
    dat %>% mutate(x0 = x, y0 = y, r = row_height / 2 * value)
  })

  # gather rect data
  rect_data <- geom_data_processor("rect", identity)

  # gather funkyrect data
  funkyrect_data <- geom_data_processor("funkyrect", function(dat) {
    dat %>%
      transmute(xmin, xmax, ymin, ymax, value) %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8)
  })

  if (nrow(funkyrect_data) > 0) {
    # offload circles in funkyrect to circles
    funkyrect_data <- funkyrect_data %>% mutate(is_circle = !is.na(start) & start < 1e-10 & 2 * pi - 1e-10 < end)
    circle_data <-
      bind_rows(
        circle_data,
        funkyrect_data %>% filter(is_circle) %>% select(x0 = x, y0 = y, r, colour)
      )
    funkyrect_data <- funkyrect_data %>% filter(!is_circle)
  }

  # gather bar data
  bar_data <- geom_data_processor("bar", function(dat) {
    if (!"opt_hjust" %in% colnames(dat)) {
      dat$opt_hjust <- NA
    }
    dat %>% mutate(
      opt_hjust = ifelse(is.na(opt_hjust), 0, opt_hjust),
      xmin = xmin + (1 - value) * xwidth * opt_hjust,
      xmax = xmax - (1 - value) * xwidth * (1 - opt_hjust)
    )
  })

  rect_data <-
    bind_rows(
      rect_data,
      bar_data
    )

  # gather text data
  text_data <- geom_data_processor("text", function(dat) {
    dat %>% mutate(
      colour = "black" # colour is overridden if !is.na(palette)
    )
  })

  # gather pie data
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
  }) %>% filter(1e-10 <= pct)

  if (nrow(pie_data) > 0) {
    # plot 100% pies as circles
    circle_data <- bind_rows(
      circle_data,
      pie_data %>% filter(pct >= (1-1e-10))
    )
    pie_data <- pie_data %>% filter(pct < (1-1e-10))
  }

  barguides_data <- geom_data_processor("bar", function(dat) {
    crossing(
      dat %>% group_by(column_id) %>% slice(1) %>% ungroup() %>% select(xmin, xmax) %>% gather(col, x) %>% select(-col),
      row_pos %>% select(y = ymin, yend = ymax)
    ) %>%
      mutate(palette = NA, value = NA)
  })

  trajd <- geom_data_processor("traj", function(dat) {
    dat %>% mutate(topinf = gsub("^gray_", "", value), colour = ifelse(grepl("^gray_", value), "lightgray", NA))
  })


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
  ###  ADD ANNOTATION TO GEOM DATA ###
  ####################################

  # GENERATE ROW ANNOTATION
  if (plot_row_annotation) {
    row_annotation <-
      row_groups %>%
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
        xmin = (levelmatch - max(levelmatch) - 1) * (col_width + col_space) - row_annot_offset,
        xmax = xmin + 1,
        x = (xmin + xmax) / 2
      ) %>%
      filter(!is.na(name), name != "")

    segment_data <-
      bind_rows(
        segment_data,
        row_annotation %>% transmute(x = xmax, xend = xmax, y = ymin, yend = ymax)
      )

    text_data <-
      bind_rows(
        text_data,
        row_annotation %>%
          mutate(xmax = xmax - .1, ymin = y, ymax = y, label = name, opt_hjust = 0.5, opt_vjust = 0, opt_fontface = "bold", opt_angle = 90, lineheight = 1.1)
      )
  }

  # GENERATE COLUMN ANNOTATION
  if (plot_column_annotation) {
    column_annotation <-
      column_groups %>%
      gather(level, name, -group, -palette) %>%
      left_join(column_pos %>% select(group, xmin, xmax), by = "group") %>%
      group_by(level, name, palette) %>%
      summarise(
        xmin = min(xmin),
        xmax = max(xmax),
        x = (xmin + xmax) / 2
      ) %>%
      ungroup() %>%
      mutate(
        levelmatch = match(level, colnames(column_groups)),
        ymin = (max(levelmatch) - levelmatch + 1) * (row_height + row_space) + col_annot_offset,
        ymax = ymin + 1,
        y = (ymin + ymax) / 2
      ) %>%
      filter(!is.na(name), name != "") %>%
      mutate(colour = palette_list$column_annotation[palette])

      rect_data <-
        bind_rows(
          rect_data,
          column_annotation %>% transmute(xmin, xmax, ymin, ymax, colour, alpha = ifelse(levelmatch == 1, 1, .25), border = FALSE)
        )

      text_data <-
        bind_rows(
          text_data,
          column_annotation %>%
            transmute(
              xmin, xmax, ymin, ymax, opt_hjust = 0.5, opt_vjust = 0.5,
              opt_fontface = ifelse(levelmatch == 1, "bold", NA),
              colour = ifelse(levelmatch == 1, "white", "black"),
              label = name
            )
        )
  }

  # ADD COLUMN NAMES
  df <- column_pos %>% filter(name != "")
  if (nrow(df) > 0) {
    segment_data <-
      bind_rows(
        segment_data,
        df %>% transmute(x = x, xend = x, y = -.3, yend = -.1, size = .5)
      )
    text_data <-
      bind_rows(
        text_data,
        df %>% transmute(
          xmin = x,
          xmax = x,
          ymin = 0,
          ymax = 0,
          opt_angle = 30,
          opt_vjust = 0,
          opt_hjust = 0,
          label = label_long(name)
        )
      )
  }

  # FIGURE OUT LEGENDS
  if (!"opt_legend" %in% colnames(column_info)) column_info$opt_legend <- NA
  legends_to_plot <-
    column_info %>%
    mutate(opt_legend = is.na(opt_legend) | opt_legend) %>%
    filter(!is.na(palette), opt_legend) %>%
    group_by(palette) %>%
    arrange(desc(opt_legend)) %>%
    slice(1) %>%
    ungroup() %>%
    select(palette, group, geom)

  ####################################
  ###         COMPOSE PLOT         ###
  ####################################

  g <-
    ggplot() +
    coord_equal(expand = FALSE) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_size_identity() +
    cowplot::theme_nothing()

  # ADD SEPARATOR LINES
  df <- row_pos %>% filter(colour_background)
  if (nrow(df) > 0) {
    g <- g + geom_rect(aes(xmin = min(column_pos$xmin)-.25, xmax = max(column_pos$xmax)+.25, ymin = ymin - (row_space / 2), ymax = ymax + (row_space / 2)), df, fill = "#EEEEEE")
  }

  # ADD BAR GUIDES
  if (nrow(barguides_data) > 0) {
    g <- g + geom_segment(aes(x = x, xend = x, y = y, yend = yend), barguides_data, colour = "black", size = .5, linetype = "dashed")
  }

  # ADD SEGMENTS
  if (nrow(segment_data) > 0) {
    if (!"size" %in% colnames(segment_data)) segment_data$size <- NA
    if (!"colour" %in% colnames(segment_data)) segment_data$colour <- NA
    if (!"linetype" %in% colnames(segment_data)) segment_data$linetype <- NA

    segment_data <- segment_data %>%
      mutate(
        size = ifelse(is.na(size), 0.5, size),
        colour = ifelse(is.na(colour), "black", colour),
        linetype = ifelse(is.na(linetype), "solid", linetype)
      )

    g <- g + geom_segment(aes(x = x, xend = xend, y = y, yend = yend, size = size, colour = colour, linetype = linetype), segment_data)
  }

  # ADD RECTANGLES
  if (nrow(rect_data) > 0) {
    if (!"alpha" %in% colnames(rect_data)) rect_data$alpha <- NA
    if (!"border" %in% colnames(rect_data)) rect_data$border <- NA

    rect_data <- rect_data %>%
      mutate(
        alpha = ifelse(is.na(alpha), 1, alpha),
        border = ifelse(is.na(border), TRUE, border),
        border_colour = ifelse(border, "black", NA)
      )
    g <- g + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour, colour = border_colour, alpha = alpha), rect_data, size = .25)
  }

  # ADD CIRCLES
  if (nrow(circle_data) > 0) {
    g <- g + ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = colour, r = r), circle_data, size = .25)
  }

  # ADD FUNKY RECTANGLES
  if (nrow(funkyrect_data) > 0) {
    funky_poly_data <- funkyrect_data %>% filter(is.na(r))
    funky_arc_data <- funkyrect_data %>% filter(!is.na(r))

    g <- g +
      geom_polygon(aes(x, y, group = name, fill = colour), funky_poly_data) +
      ggforce::geom_arc_bar(aes(x0 = x, y0 = y, r0 = 0, r = r, start = start, end = end, fill = colour), funky_arc_data, colour = NA) +
      geom_path(aes(x = x, y = y, group = paste0(name, "_", subgroup)), funky_poly_data, colour = "black", size = .25) +
      geom_arc(aes(x0 = x, y0 = y, r = r, start = start, end = end), funky_arc_data, colour = "black", size = .25)
  }

  # ADD STARS
  if (nrow(pie_data) > 0) {
    g <- g + ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = colour), data = pie_data, size = .25)
  }

  # ADD TEXT
  if (nrow(text_data) > 0) {
    if (!"opt_hjust" %in% colnames(text_data)) text_data$opt_hjust <- NA
    if (!"opt_vjust" %in% colnames(text_data)) text_data$opt_vjust <- NA
    if (!"opt_size" %in% colnames(text_data)) text_data$opt_size <- NA
    if (!"opt_fontface" %in% colnames(text_data)) text_data$opt_fontface <- NA
    if (!"opt_angle" %in% colnames(text_data)) text_data$opt_angle <- NA
    if (!"colour" %in% colnames(text_data)) text_data$colour <- NA
    if (!"lineheight" %in% colnames(text_data)) text_data$lineheight <- NA

    text_data <- text_data %>% mutate(
      opt_hjust = ifelse(is.na(opt_hjust), .5, opt_hjust),
      opt_vjust = ifelse(is.na(opt_vjust), .5, opt_vjust),
      opt_size = ifelse(is.na(opt_size), 4, opt_size),
      opt_fontface = ifelse(is.na(opt_fontface), "plain", opt_fontface),
      opt_angle = ifelse(is.na(opt_angle), 0, opt_angle),
      colour = ifelse(is.na(colour), "black", colour),
      lineheight = ifelse(is.na(lineheight), 1.2, lineheight),
      x = (1 - opt_hjust) * xmin + opt_hjust * xmax,
      y = (1 - opt_vjust) * ymin + opt_vjust * ymax
    ) %>%
      filter(label != "")

    g <- g + geom_text(aes(x = x, y = y, label = label, colour = colour, hjust = opt_hjust, vjust = opt_vjust, size = opt_size, fontface = opt_fontface, angle = opt_angle), data = text_data)
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


  # ADD SIZE
  suppressWarnings({
    minimum_x <- min(column_pos$xmin, segment_data$x, segment_data$xend, rect_data$xmin, circle_data$x - circle_data$r, funkyrect_data$x - funkyrect_data$r, pie_data$xmin, text_data$xmin, na.rm = TRUE)
    maximum_x <- max(column_pos$xmax, segment_data$x, segment_data$xend, rect_data$xmax, circle_data$x + circle_data$r, funkyrect_data$x + funkyrect_data$r, pie_data$xmax, text_data$xmax, na.rm = TRUE)
    minimum_y <- min(row_pos$ymin, segment_data$y, segment_data$yend, rect_data$ymin, circle_data$y - circle_data$r, funkyrect_data$y - funkyrect_data$r, pie_data$ymin, text_data$ymin, na.rm = TRUE)
    maximum_y <- max(row_pos$ymax, segment_data$y, segment_data$yend, rect_data$ymax, circle_data$y + circle_data$r, funkyrect_data$y + funkyrect_data$r, pie_data$ymax, text_data$ymax, na.rm = TRUE)
  })

  # reserve a bit more room for text that wants to go outside the frame
  minimum_x <- minimum_x - 2
  maximum_x <- maximum_x + 2
  minimum_y <- minimum_y - 2
  maximum_y <- maximum_y + 2

  g$width <- maximum_x - minimum_x
  g$height <- maximum_y - minimum_y

  g <- g + expand_limits(x = c(minimum_x, maximum_x), y = c(minimum_y, maximum_y))

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

process_geom_params <- function(column_info) {
  bind_cols(
    column_info %>% select(-options),
    column_info %>% pull(options) %>% map_df(function(l) {
      if (length(l) == 0 || (length(l) == 1) && is.na(l)) {
        tibble(a = 1)[,integer(0)]
      } else {
        names(l) <- paste0("opt_", names(l))
        as_data_frame(l)
      }
    })
  )
}

make_geom_data_processor <- function(data, column_pos, row_pos, scale_column, palette_list) {
  function(geom_types, fun) {
    column_sels <-
      column_pos %>%
      filter(geom %in% geom_types) %>%
      select(-group, -name, -do_spacing) %>%
      rename(column_id = id)

    if (nrow(column_sels) == 0) {
      return(tibble(a = 1) %>% slice(integer()))
    }

    map_df(seq_len(nrow(column_sels)), function(ri) {
      # cat("Processing ", ri, "\n", sep = "")
      column_sel <- column_sels %>% slice(ri)

      if (!"opt_label" %in% colnames(column_sel)) {
        column_sel$opt_label <- NA
      }

      column_sel <-
        column_sel %>%
        mutate(opt_label = ifelse(geom == "text" & is.na(opt_label), column_id, opt_label))

      row_sel <-
        row_pos %>%
        select(row_id = id, ysep, y, ymin, ymax)

      data_sel <-
        data %>%
        select(row_id = id, !!column_sel$column_id) %>%
        gather(column_id, value, -row_id)

      labelcolumn_sel <-
        column_sel %>%
        filter(!is.na(opt_label))

      if (nrow(labelcolumn_sel) > 0) {
        label_sel <-
          data %>%
          select(row_id = id, !!labelcolumn_sel$opt_label) %>%
          gather(opt_label, label, -row_id) %>%
          left_join(labelcolumn_sel %>% select(opt_label, column_id), by = "opt_label") %>%
          select(-opt_label)
        data_sel <-
          left_join(data_sel, label_sel, by = c("row_id", "column_id"))
      }

      dat <-
        data_sel %>%
        left_join(column_sel, by = "column_id") %>%
        left_join(row_sel, by = "row_id")

      # scale data, if need be
      if (scale_column && is.numeric(dat$value)) {
        dat <-
          dat %>%
          group_by(column_id) %>%
          mutate(value = dynutils::scale_minmax(value)) %>%
          ungroup()
      }

      # apply function
      dat <- fun(dat)

      # determine colours
      if (!is.na(column_sel$palette)) {
        palette_sel <- palette_list[[column_sel$palette]]

        if (is.character(dat$value)) {
          dat <- dat %>% mutate(col_value = value)
        } else if (is.numeric(dat$value)) {
          dat <- dat %>% mutate(col_value = round(value * (length(palette_sel) - 1)) + 1)
        } else {
          dat$col_value <- NA
        }

        dat <- dat %>%
          mutate(colour = ifelse(is.na(col_value), "#444444FF", palette_sel[col_value])) %>%
          select(-value, -col_value)
      }

      dat
    })
  }
}


rounded_rectangle <- function(xmin, xmax, ymin, ymax, corner_size) {
  # rename for ease of use
  cs <- corner_size

  arc_bars <-
    tribble(
      ~x, ~y, ~r, ~start, ~end,
      xmax - cs, ymax - cs, cs, 0, pi / 2,
      xmax - cs, ymin + cs, cs, pi / 2, pi,
      xmin + cs, ymin + cs, cs, pi, pi * 3 / 2,
      xmin + cs, ymax - cs, cs, pi * 3 / 2, 2 * pi
    )

  polygons <-
    tribble(
      ~x, ~y, ~subgroup,
      xmin + cs, ymin, "bottom",
      xmax - cs, ymin, "bottom",
      xmax, ymin + cs, "right",
      xmax, ymax - cs, "right",
      xmax - cs, ymax, "top",
      xmin + cs, ymax, "top",
      xmin, ymax - cs, "left",
      xmin, ymin + cs, "left"
    )

  lst(arc_bars, polygons)
}

score_to_funky_rectangle <- function(xmin, xmax, ymin, ymax, value, midpoint = .5, name = NULL) {
  if (is.na(value)) {
    return(NULL)
  }

  if (is.null(name)) {
    name <- dynutils::random_time_string()
  }

  out <-
    if (value >= midpoint) {
      # transform value to a 0.5 .. 1.0 range
      trans <- (value - midpoint) / (1 - midpoint) / 2 + .5
      corner_size <- .9 - .8 * trans

      rounded_rectangle(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        corner_size = corner_size
      )
    } else {
      # transform value to a 0.0 .. 0.5 range
      trans <- value / midpoint / 2

      data_frame(
        x = xmin / 2 + xmax / 2,
        y = ymin / 2 + ymax / 2,
        r = trans * .9 + .1,
        start = 0,
        end = 2 * pi
      )
    }

  bind_rows(out) %>%
    mutate(name, value)
}
