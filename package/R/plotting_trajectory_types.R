#' Plot a trajectory type
#'
#' @param plot Existing plot
#' @param trajectory_types One or more trajectory types
#' @param xmins Minimal x bound
#' @param xmaxs Maximal y bound
#' @param ymins Minimal y bound
#' @param ymaxs Maximal y bound
#' @param size Size of the arrows and nodes
#' @param geom Which underlying geom to use. When using `"point"`, it is expected to also be
#'   using `scale_colour_identity()` with your plot. The nodes will go slightly outside the (x, y) boundaries,
#'   because the size of the points depend on the plot size. When using `"circle"`, it is expected to be using
#'   `scale_fill_identity()` and `coord_equal()`, otherwise the nodes might be elliptical.`
#' @param circ_size Size of nodes, only used when `geom == "circle"`.
#'
#' @importFrom ggforce geom_circle
#'
#' @export
plot_trajectory_types <- function(
  plot = ggplot() + theme_void(),
  trajectory_types,
  xmins = 0,
  xmaxs = 1,
  ymins = 0,
  ymaxs = 1,
  size = 4,
  geom = c("point", "circle"),
  circ_size = ifelse(geom == "circle", .05, 0)
) {
  # make sure all variables have the same length
  if (length(xmins) == 1) xmins <- rep(xmins, length(trajectory_types))
  if (length(xmaxs) == 1) xmaxs <- rep(xmaxs, length(trajectory_types))
  if (length(ymins) == 1) ymins <- rep(ymins, length(trajectory_types))
  if (length(ymaxs) == 1) ymaxs <- rep(ymaxs, length(trajectory_types))

  testthat::expect_true(length(unique(c(length(trajectory_types), length(xmins), length(xmaxs), length(ymins), length(ymaxs)))) == 1)

  geom <- match.arg(geom)

  # loop over every trajectory type and plot
  # for (i in seq_along(trajectory_types)) {
  out <- map(seq_along(trajectory_types), function(i) {
    trajectory_type <- trajectory_types[[i]]
    xmin <- xmins[[i]] + circ_size
    xmax <- xmaxs[[i]] - circ_size
    ymin <- ymins[[i]] + circ_size
    ymax <- ymaxs[[i]] - circ_size

    trajectory_type_info <- dynutils::extract_row_to_list(dynwrap::trajectory_types, which(dynwrap::trajectory_types$id == trajectory_type))

    network <- trajectory_type_info$example_network
    nodes <- trajectory_type_info$example_nodes %>% mutate(id = row_number())

    # change positions of nodes based on bounds
    nodes$x <- nodes$x * (xmax - xmin) / 6 + xmin
    nodes$y <- nodes$y * (ymax - ymin) / 4 + ymin
    nodes$colour <- trajectory_type_info$colour
    nodes$r <- circ_size

    network_positions <- network %>%
      left_join(nodes, c("from" = "id")) %>%
      left_join(nodes %>% rename_all(~paste0(., "end")), c("to" = "idend")) %>%
      mutate(xmid = x + (xend - x) / 1.5, ymid = y + (yend - y) / 1.5)


    lst(
      nodes,
      network_positions
    )
  })

  nodes <- map_df(out, ~ .$nodes)
  network_positions <- map_df(out, ~ .$network_positions)

  plot <- plot +
    geom_segment(
      aes(x = x, y = y, xend = xend, yend = yend),
      data = network_positions,
      size = size / 3
    ) +
    geom_segment(
      aes(x = x, y = y, xend = xmid, yend = ymid),
      data = network_positions,
      size = size / 3,
      arrow = arrow(type = "closed", length = unit(0.04 * size, "inches"))
    )

  if (geom == "circle") {
    plot +
      ggforce::geom_circle(aes(x0 = x, y0 = y, r = r, fill = colour), data = nodes, colour = NA)
  } else if (geom == "point") {
    plot +
      geom_point(aes(x, y, color = colour), data = nodes, size = size)
  }

}
