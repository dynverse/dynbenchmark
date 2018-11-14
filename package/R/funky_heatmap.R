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
#' @param add_abc Whether or not to add subfigure labels to the different columns groups.
#' @param col_annot_offset How much the column annotation will be offset by.
#' @param row_annot_offset How much the column annotation will be offset by.
#' @param removed_methods Which methods to not show in the rows
#'
#' @importFrom ggforce geom_arc_bar geom_circle geom_arc
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
  add_abc = TRUE,
  col_annot_offset = 3,
  row_annot_offset = .5,
  removed_methods = NULL
) {
  # no point in making these into parameters
  row_height <- 1
  row_space <- .1
  row_bigspace <- .5
  col_width <- 1
  col_space <- .1
  col_bigspace <- .5

  # DETERMINE ROW POSITIONS
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
      ysep = ifelse(do_spacing, row_height + 2 * row_space, row_space),
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

  column_info <-
    column_info %>%
    process_geom_params() %>%
    add_column_if_missing(width = col_width, overlay = FALSE, legend = TRUE)

  column_pos <-
    column_info %>%
    mutate(
      do_spacing = c(FALSE, diff(as.integer(factor(group))) != 0),
      xsep = case_when(
        overlay ~ c(0, -head(width, -1)),
        do_spacing ~ col_bigspace,
        TRUE ~ col_space
      ),
      xwidth = case_when(
        overlay & width < 0 ~ width - xsep,
        overlay ~ -xsep,
        TRUE ~ width
      ),
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
  segment_data <- NULL # placeholder for if this ever gets used

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

  # gather bar data
  bar_data <- geom_data_processor("bar", function(dat) {
    dat %>%
      add_column_if_missing(hjust = 0) %>%
      mutate(
        xmin = xmin + (1 - value) * xwidth * hjust,
        xmax = xmax - (1 - value) * xwidth * (1 - hjust)
      )
  })

  # gather bar guides data
  barguides_data <- geom_data_processor("bar", function(dat) {
    crossing(
      dat %>% group_by(column_id) %>% slice(1) %>% ungroup() %>% select(xmin, xmax) %>% gather(col, x) %>% transmute(x, xend = x),
      row_pos %>% select(y = ymin, yend = ymax)
    ) %>%
      mutate(palette = NA, value = NA)
  })
  segment_data <-
    bind_rows(
      segment_data,
      barguides_data %>% mutate(colour = "black", size = .5, linetype = "dashed")
    )

  # gather text data
  text_data <- geom_data_processor("text", function(dat) {
    dat %>% mutate(colour = "black") # colour "black" is overridden if !is.na(palette)
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

  # hidden feature trajectory plots
  trajd <- geom_data_processor("traj", function(dat) {
    dat %>% mutate(topinf = gsub("^gray_", "", value), colour = ifelse(grepl("^gray_", value), "#AAAAAA", NA))
  })

  ####################################
  ###  ADD ANNOTATION TO GEOM DATA ###
  ####################################

  # GENERATE ROW ANNOTATION
  if (plot_row_annotation) {
    row_annotation <-
      row_groups %>%
      gather(level, name, -group) %>%
      left_join(row_pos %>% select(group, ymin, ymax), by = "group") %>%
      group_by(name) %>%
      summarise(
        ymin = min(ymin),
        ymax = max(ymax),
        y = (ymin + ymax) / 2
      ) %>%
      ungroup() %>%
      mutate(xmin = -.5, xmax = 5) %>%
      filter(!is.na(name), name != "")

    text_data <- text_data %>% bind_rows(
      row_annotation %>%
        transmute(xmin, xmax, ymin = ymax + row_space, label_value = name %>% gsub("\n", " ", .), hjust = 0, vjust = .5, fontface = "bold") %>%
        mutate(ymax = ymin + row_height)
    )
  }

  # GENERATE COLUMN ANNOTATION
  if (plot_column_annotation) {
    col_join <- column_groups %>%
      gather(level, name, -group, -palette) %>%
      left_join(column_pos %>% select(group, xmin, xmax), by = "group")

    text_pct <- .9
    level_heights <-
      col_join %>%
      group_by(level) %>%
      summarise(max_newlines = max(str_count(name, "\n"))) %>%
      mutate(
        height = (max_newlines + 1) * text_pct + (1 - text_pct),
        levelmatch = match(level, colnames(column_groups))
      ) %>%
      arrange(desc(levelmatch)) %>%
      mutate(
        ysep = row_space,
        ymax = col_annot_offset + cumsum(height + ysep) - ysep,
        ymin = ymax - height,
        y = (ymin + ymax) / 2
      )

    column_annotation <-
      col_join %>%
      group_by(level) %>%
      mutate(max_newlines = max(str_count(name, "\n"))) %>%
      group_by(level, name, palette) %>%
      summarise(
        xmin = min(xmin),
        xmax = max(xmax),
        x = (xmin + xmax) / 2
      ) %>%
      ungroup() %>%
      left_join(level_heights, by = "level") %>%
      filter(!is.na(name), grepl("[A-Za-z]", name)) %>%
      mutate(colour = palette_list$column_annotation[palette])

    rect_data <- rect_data %>% bind_rows(
      column_annotation %>%
        transmute(xmin, xmax, ymin, ymax, colour, alpha = ifelse(levelmatch == 1, 1, .25), border = FALSE)
    )

    text_data <- text_data %>% bind_rows(
      column_annotation %>%
        transmute(
          xmin, xmax, ymin, ymax,
          hjust = 0.5, vjust = 0.5,
          fontface = ifelse(levelmatch == 1, "bold", NA),
          colour = ifelse(levelmatch == 1, "white", "black"),
          label_value = name
        )
    )

    if (add_abc) {
      text_data <- text_data %>% bind_rows(
        column_annotation %>%
          filter(levelmatch == 1) %>%
          arrange(x) %>%
          transmute(
            xmin = xmin + col_space, xmax = xmax - col_space, ymin, ymax,
            hjust = 0, vjust = 0.5,
            fontface = "bold",
            colour = "white",
            label_value = paste0(letters[row_number()], ")")
          )
      )
    }
  }

  # ADD COLUMN NAMES
  df <- column_pos %>% filter(name != "")
  if (nrow(df) > 0) {
    segment_data <- segment_data %>% bind_rows(
      df %>% transmute(x = x, xend = x, y = -.3, yend = -.1, size = .5)
    )
    text_data <-
      bind_rows(
        text_data,
        df %>% transmute(
          xmin = x, xmax = x, ymin = 0, ymax = 0,
          angle = 30, vjust = 0, hjust = 0,
          label_value = label_long(name)
        )
      )
  }

  # determine dsize of current geoms
  suppressWarnings({
    minimum_x <- min(column_pos$xmin, segment_data$x, segment_data$xend, rect_data$xmin, circle_data$x - circle_data$r, funkyrect_data$x - funkyrect_data$r, pie_data$xmin, text_data$xmin, na.rm = TRUE)
    maximum_x <- max(column_pos$xmax, segment_data$x, segment_data$xend, rect_data$xmax, circle_data$x + circle_data$r, funkyrect_data$x + funkyrect_data$r, pie_data$xmax, text_data$xmax, na.rm = TRUE)
    minimum_y <- min(row_pos$ymin, segment_data$y, segment_data$yend, rect_data$ymin, circle_data$y - circle_data$r, funkyrect_data$y - funkyrect_data$r, pie_data$ymin, text_data$ymin, na.rm = TRUE)
    maximum_y <- max(row_pos$ymax, segment_data$y, segment_data$yend, rect_data$ymax, circle_data$y + circle_data$r, funkyrect_data$y + funkyrect_data$r, pie_data$ymax, text_data$ymax, na.rm = TRUE)
  })

  ####################################
  ###   CREATE HARDCODED LEGENDS   ###
  ####################################

  legend_pos <- minimum_y

  # CREATE LEGENDS
  if (any(column_pos$geom == "pie")) {
    rel_cols <- column_pos %>% filter(geom == "pie") %>% arrange(x) %>% group_by(palette) %>% slice(1) %>% ungroup()

    for (i in seq_len(nrow(rel_cols))) {
      palette <- palette_list[[rel_cols$palette[[i]]]]

      pie_minimum_x <- rel_cols$xmin[[i]]

      pie_legend_df <-
        palette %>%
        enframe(value = "fill") %>%
        mutate(
          rad_start = seq(0, pi, length.out = n() + 1) %>% head(-1),
          rad_end = seq(0, pi, length.out = n() + 1) %>% tail(-1),
          rad = (rad_start + rad_end) / 2,
          colour = rep("black", length(rad)),
          lab_x = row_height * sin(rad),
          lab_y = seq(row_height * (cos(first(rad)) + .2), row_height * (cos(last(rad)) - .2), length.out = n()),
          hjust = rep(0, length(rad)),
          xpt = row_height * sin(rad),
          ypt = row_height * cos(rad),
          vjust = .5
        )

      pie_title_data <-
        data_frame(xmin = pie_minimum_x, xmax = pie_minimum_x, ymin = legend_pos - 1.5, ymax = legend_pos - .5, label_value = "Error reason", hjust = 0, vjust = 1, fontface = "bold")

      pie_pie_data <-
        pie_legend_df %>%
        transmute(x0 = pie_minimum_x, y0 = legend_pos - 2.75, r0 = 0, r = row_height * .75, rad_start, rad_end, colour = fill)

      pie_text_data <-
        pie_legend_df %>%
        transmute(x = pie_minimum_x + .5 + lab_x, y = legend_pos - 2.75 + lab_y, label_value = name, vjust, hjust, colour) %>%
        mutate(xmin = x, xmax = x, ymin = y - .4, ymax = y + .4)

      pie_seg_data <-
        pie_legend_df %>%
        transmute(x = pie_minimum_x + xpt * .85, xend = pie_minimum_x + xpt * 1.1, y = legend_pos - 2.75 + ypt * .85, yend = legend_pos - 2.75 + ypt * 1.1)

      text_data <- text_data %>% bind_rows(
        pie_title_data,
        pie_text_data
      )

      pie_data <- pie_data %>% bind_rows(
        pie_pie_data
      )

      segment_data <- segment_data %>% bind_rows(
        pie_seg_data
      )
    }
  }

  # funkyrect legend
  if (any(column_pos$geom == "funkyrect")) {
    fr_minimum_x <- column_pos %>% filter(geom == "funkyrect") %>% pull(xmin) %>% min

    fr_legend_size <- 1
    fr_legend_space <- .2

    fr_legend_dat1 <-
      data_frame(
        value = seq(0, 1, by = .1),
        xmin = 0, xmax = col_width * fr_legend_size,
        ymin = 0, ymax = col_width * fr_legend_size
      )

    fr_poly_data1 <-
      fr_legend_dat1 %>%
      transmute(xmin, xmax, ymin, ymax, value) %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8)

    fr_legend_dat2 <-
      fr_poly_data1 %>%
      filter(!is.na(r)) %>%
      group_by(value) %>%
      summarise(minx = min(x - r), maxx = max(x + r), miny = min(y - r), maxy = max(y + r)) %>%
      mutate(
        width = maxx - minx,
        height = maxy - miny,
        xmin = cumsum(width + fr_legend_space) - width - fr_legend_space,
        xmin = fr_minimum_x + xmin - min(xmin),
        xmax = xmin + width,
        ymin = legend_pos - 2.5,
        ymax = ymin + height
      ) %>%
      transmute(
        width, height,
        value, xmin, xmax, ymin, ymax,
        x = (xmin + xmax) / 2, y = (ymin + ymax) / 2
      )

    fr_maximum_x <- max(fr_legend_dat2$xmax)

    fr_poly_data2 <-
      transmute(fr_legend_dat2, xmin = x - fr_legend_size / 2, xmax = x + fr_legend_size / 2, ymin = y - fr_legend_size / 2, ymax = y + fr_legend_size / 2, value) %>%
      pmap_df(score_to_funky_rectangle, midpoint = .8) %>%
      mutate(
        col_value = round(value * (length(palette_list$overall) - 1)) + 1,
        colour = ifelse(is.na(col_value), "#444444FF", palette_list$overall[col_value])
      )

    fr_title_data <-
      data_frame(xmin = fr_minimum_x, xmax = fr_maximum_x, ymin = legend_pos - 1.5, ymax = legend_pos - .5, label_value = "Score", hjust = 0, vjust = 1, fontface = "bold")

    fr_value_data <-
      fr_legend_dat2 %>% filter(value %% .2 == 0) %>% transmute(
        ymin = ymin - 1,
        ymax = ymin,
        xmin, xmax,
        hjust = .5,
        vjust = 0,
        label_value = ifelse(value %in% c(0, 1), sprintf("%.0f", value), sprintf("%.1f", value))
      )

    text_data <- bind_rows(
      text_data,
      fr_title_data,
      fr_value_data
    )
    funkyrect_data <- bind_rows(funkyrect_data, fr_poly_data2)
  }

  if (any(column_pos$id == "method_priors_required_str")) {
    pr_minimum_x <- column_pos %>% filter(id == "method_priors_required_str") %>% pull(xmin) %>% min

    legend_vals <- tribble(
      ~symbol, ~value,
      "", "None",
      "\u2715", "Weak: Start or end cells",
      "\u2716", "Strong: Cell grouping or time course"
    )

    pr_labels_df <-
      legend_vals %>%
      mutate(
        lab_x1 = pr_minimum_x,
        lab_x2 = pr_minimum_x + 1,
        lab_y = legend_pos - 1 - row_number() * row_height * .9
      )

    pr_text_data <-
      bind_rows(
        data_frame(xmin = pr_minimum_x, xmax = pr_minimum_x, ymin = legend_pos - 1.5, ymax = legend_pos - .5, label_value = "Prior information required", hjust = 0, vjust = 1, fontface = "bold"),
        pr_labels_df %>% transmute(xmin = lab_x1, xmax = lab_x1, ymin = lab_y, ymax = lab_y, label_value = symbol, hjust = 0, vjust = 0),
        pr_labels_df %>% transmute(xmin = lab_x2, xmax = lab_x2, ymin = lab_y, ymax = lab_y, label_value = value, hjust = 0, vjust = 0)
      )

    text_data <- text_data %>% bind_rows(
      pr_text_data
    )
  }

  if (!is.null(removed_methods)) {
    # rm_min_x <- column_pos %>% filter(!group %in% c("method_characteristic", "inferrable_trajtype", "benchmark_metric", "benchmark_source")) %>% slice(1) %>% pull(xmin)
    rm_min_x <- 20

    num_cols <- 2
    num_rows <- ceiling(length(removed_methods) / num_cols)

    rm_lab_df <-
      data_frame(label_value = removed_methods) %>%
      mutate(
        row = (row_number() - 1) %% num_rows,
        col = ceiling(row_number()  / num_rows) - 1,
        x = rm_min_x + col * 5,
        y = legend_pos - (row + 2) * row_height * .9
      )
    rm_text_data <-
      bind_rows(
        data_frame(xmin = rm_min_x, xmax = rm_min_x, ymin = legend_pos - 1.5, ymax = legend_pos - .5, label_value = "Not shown, insufficient data points", hjust = 0, vjust = 1, fontface = "bold"),
        rm_lab_df %>% mutate(xmin = x, xmax = x, ymin = y, ymax = y, hjust = 0, vjust = 0)
      )

    text_data <- text_data %>% bind_rows(
      rm_text_data
    )
  }

  minimum_y <- min(minimum_y, min(text_data$ymin, na.rm = TRUE))

  ####################################
  ###    SIMPLIFY CERTAIN GEOMS    ###
  ####################################

  # small funkyrects are circles
  if (nrow(funkyrect_data) > 0) {
    funkyrect_data <- funkyrect_data %>% mutate(is_circle = !is.na(start) & start < 1e-10 & 2 * pi - 1e-10 < end)
    circle_data <- circle_data %>% bind_rows(
      funkyrect_data %>% filter(is_circle) %>% select(x0 = x, y0 = y, r, colour)
    )
    funkyrect_data <- funkyrect_data %>% filter(!is_circle)
  }

  # bars are rects
  rect_data <-
    bind_rows(
      rect_data,
      bar_data
    )
  bar_data <- NULL

  # 100% pies are circles
  if (nrow(pie_data) > 0) {
    pie_data <- pie_data %>% mutate(pct = (rad_end - rad_start) / 2 / pi)
    # plot 100% pies as circles
    circle_data <- bind_rows(
      circle_data,
      pie_data %>% filter(pct >= (1-1e-10))
    )
    pie_data <- pie_data %>% filter(pct < (1-1e-10))
  }

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
    scale_linetype_identity() +
    cowplot::theme_nothing()

  # PLOT ROW BACKGROUNDS
  df <- row_pos %>% filter(colour_background)
  if (nrow(df) > 0) {
    g <- g + geom_rect(aes(xmin = min(column_pos$xmin)-.25, xmax = max(column_pos$xmax)+.25, ymin = ymin - (row_space / 2), ymax = ymax + (row_space / 2)), df, fill = "#DDDDDD")
  }

  # PLOT SEGMENTS
  if (nrow(segment_data) > 0) {
    # add defaults for optional values
    segment_data <- segment_data %>% add_column_if_missing(size = .5, colour = "black", linetype = "solid")

    g <- g + geom_segment(aes(x = x, xend = xend, y = y, yend = yend, size = size, colour = colour, linetype = linetype), segment_data)
  }

  # PLOT RECTANGLES
  if (nrow(rect_data) > 0) {
    # add defaults for optional values
    rect_data <- rect_data %>%
      add_column_if_missing(alpha = 1, border = TRUE, border_colour = "black") %>%
      mutate(border_colour = ifelse(border, border_colour, NA))

    g <- g + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour, colour = border_colour, alpha = alpha), rect_data, size = .25)
  }

  # PLOT CIRCLES
  if (nrow(circle_data) > 0) {
    g <- g + ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = colour, r = r), circle_data, size = .25)
  }

  # PLOT FUNKY RECTANGLES
  if (nrow(funkyrect_data) > 0) {
    # there are polygons and there are quarter-circles to be plotted
    # it's possible to distinguish one from another by looking at the 'r' column
    funky_poly_data <- funkyrect_data %>% filter(is.na(r))
    funky_arc_data <- funkyrect_data %>% filter(!is.na(r))

    g <- g +
      # plot polygon fill
      geom_polygon(aes(x, y, group = name, fill = colour), funky_poly_data) +
      # plot quarter circle fill
      ggforce::geom_arc_bar(aes(x0 = x, y0 = y, r0 = 0, r = r, start = start, end = end, fill = colour), funky_arc_data, colour = NA) +
      # plot polygon border
      geom_path(aes(x = x, y = y, group = paste0(name, "_", subgroup)), funky_poly_data, colour = "black", size = .25) +
      # plot quarter circle border
      ggforce::geom_arc(aes(x0 = x, y0 = y, r = r, start = start, end = end), funky_arc_data, colour = "black", size = .25)
  }

  # PLOT PIES
  if (nrow(pie_data) > 0) {
    g <- g + ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = colour), data = pie_data, size = .25)
  }

  # PLOT TEXT
  if (nrow(text_data) > 0) {
    # add defaults for optional values
    text_data <- text_data %>%
      add_column_if_missing(
        hjust = .5,
        vjust = .5,
        size = 4,
        fontface = "plain",
        colour = "black",
        lineheight = 1,
        angle = 0
      ) %>%
      mutate(
        angle2 = angle / 360 * 2 * pi,
        cosa = cos(angle2) %>% round(2),
        sina = sin(angle2) %>% round(2),
        alphax = ifelse(cosa < 0, 1 - hjust, hjust) * abs(cosa) + ifelse(sina > 0, 1 - vjust, vjust) * abs(sina),
        alphay = ifelse(sina < 0, 1 - hjust, hjust) * abs(sina) + ifelse(cosa < 0, 1 - vjust, vjust) * abs(cosa),
        x = (1 - alphax) * xmin + alphax * xmax,
        y = (1 - alphay) * ymin + alphay * ymax
      ) %>%
      filter(label_value != "")

    g <- g + geom_text(aes(x = x, y = y, label = label_value, colour = colour, hjust = hjust, vjust = vjust, size = size, fontface = fontface, angle = angle), data = text_data)
  }

  # PLOT TRAJ TYPES
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
  # reserve a bit more room for text that wants to go outside the frame
  # minimum_x <- minimum_x - 2
  if ("qc_cat_documentation" %in% column_info$id) maximum_x <- maximum_x + 2
  # minimum_y <- minimum_y - 2
  # maximum_y <- maximum_y + 2

  g$width <- maximum_x - minimum_x
  g$height <- maximum_y - minimum_y

  g <- g + expand_limits(x = c(minimum_x, maximum_x), y = c(minimum_y, maximum_y))

  g
}

process_geom_params <- function(column_info) {
  bind_cols(
    column_info %>% select(-options),
    column_info %>% pull(options) %>% map_df(function(l) {
      if (length(l) == 0 || (length(l) == 1) && is.na(l)) {
        tibble(a = 1)[,integer(0)]
      } else {
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
      rename(column_id = id) %>%
      add_column_if_missing(label = NA_character_, scale = TRUE)

    if (nrow(column_sels) == 0) {
      return(tibble(a = 1) %>% slice(integer()))
    }

    map_df(seq_len(nrow(column_sels)), function(ri) {
      # cat("Processing ", ri, "\n", sep = "")
      column_sel <-
        column_sels %>%
        slice(ri) %>%
        mutate(label = ifelse(geom == "text" & is.na(label), column_id, label))

      row_sel <-
        row_pos %>%
        select(row_id = id, ysep, y, ymin, ymax)

      data_sel <-
        data %>%
        select(row_id = id, !!column_sel$column_id) %>%
        gather(column_id, value, -row_id)

      labelcolumn_sel <-
        column_sel %>%
        filter(!is.na(label))

      if (nrow(labelcolumn_sel) > 0) {
        label_sel <-
          data %>%
          select(row_id = id, !!labelcolumn_sel$label) %>%
          gather(label_column, label_value, -row_id) %>%
          left_join(labelcolumn_sel %>% select(label_column = label, column_id), by = "label_column") %>%
          select(-label_column)
        data_sel <-
          left_join(data_sel, label_sel, by = c("row_id", "column_id"))
      }

      dat <-
        data_sel %>%
        left_join(column_sel, by = "column_id") %>%
        left_join(row_sel, by = "row_id")

      # scale data, if need be
      if (scale_column && column_sel$scale && is.numeric(dat$value)) {
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
      corner_size <- (.9 - .8 * trans) * min(xmax - xmin, ymax - ymin)

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
        r = (trans * .9 + .1) * min(xmax - xmin, ymax - ymin),
        start = 0,
        end = 2 * pi
      )
    }

  bind_rows(out) %>%
    mutate(name, value)
}

add_column_if_missing <- function(df, ...) {
  column_values <- list(...)
  for (column_name in names(column_values)) {
    default_val <- rep(column_values[[column_name]], nrow(df))

    if (column_name %in% colnames(df)) {
      df[[column_name]] <- ifelse(is.na(df[[column_name]]), default_val, df[[column_name]])
    } else {
      df[[column_name]] <- default_val
    }
  }
  df
}
