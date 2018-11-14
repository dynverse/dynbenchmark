#' Helper plotting functions the complementarity experiment

quarter_circ <- function(xstart, ystart, from_angle, to_angle, size) {
  data_frame(
    angle = seq(from_angle, to_angle, length.out = 20) * 2 * pi,
    x = sin(angle),
    y = cos(angle)
  ) %>% mutate(
    x = (x - min(x)) / (max(x) - min(x)) * size + xstart,
    y = (y - min(y)) / (max(y) - min(y)) * size + ystart
  )
}

bracket <- function(xmin, xmax, ymin, ymax, flip = FALSE) {
  width <- xmax - xmin
  height <- ymax - ymin
  xmid <- xmin + width / 2
  ymid <- ymin + height / 2
  curly_size <- height / 2

  df <-
    bind_rows(
      quarter_circ(xmin, ymin, .75, 1, curly_size),
      quarter_circ(xmid - curly_size, ymid, .5, .25, curly_size),
      quarter_circ(xmid, ymid, .75, .5, curly_size),
      quarter_circ(xmax - curly_size, ymin, 0, .25, curly_size)
    )

  if (flip) {
    df <- df %>% mutate(y = ymin + ymax - y)
  }

  df
}
