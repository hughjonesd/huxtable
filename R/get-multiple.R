
get_all_borders <- function (ht, row, col, drop = TRUE) {
  list(
    left   = left_border(ht)[row, col, drop = drop],
    right  = right_border(ht)[row, col, drop = drop],
    top    = top_border(ht)[row, col, drop = drop],
    bottom = bottom_border(ht)[row, col, drop = drop]
  )
}


get_all_border_colors <- function (ht, row, col, drop = TRUE) {
  list(
    left   = left_border_color(ht)[row, col, drop = drop],
    right  = right_border_color(ht)[row, col, drop = drop],
    top    = top_border_color(ht)[row, col, drop = drop],
    bottom = bottom_border_color(ht)[row, col, drop = drop]
  )
}


get_all_border_styles <- function (ht, row, col, drop = TRUE) {
  list(
    left   = left_border_style(ht)[row, col, drop = drop],
    right  = right_border_style(ht)[row, col, drop = drop],
    top    = top_border_style(ht)[row, col, drop = drop],
    bottom = bottom_border_style(ht)[row, col, drop = drop]
  )
}


get_all_padding <- function (ht, row, col, drop = TRUE) {
  list(
    left   = left_padding(ht)[row, col, drop = drop],
    right  = right_padding(ht)[row, col, drop = drop],
    top    = top_padding(ht)[row, col, drop = drop],
    bottom = bottom_padding(ht)[row, col, drop = drop]
  )
}
