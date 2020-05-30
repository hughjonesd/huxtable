
#' Set borders and padding around a rectangle of cells
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value Border width, color, style or a [brdr()] object. See [borders].
#'   For padding, padding width in points.
#'
#' @details
#' `set_outer_borders` sets borders round the top, bottom, left and
#' right of a group of cells. Behaviour is undefined unless `row` and `col`
#' specify contiguous sequences. `set_outer_border_colors` and
#' `set_outer_border_styles` set border colors and styles. `set_outer_padding`
#' sets padding, i.e. top padding on the top row of cells, etc.
#'
#' @examples
#' ht2 <- huxtable(a = 1:3, b = 1:3)
#' set_outer_borders(ht2)
#' set_outer_borders(ht2, 2:3, 1:2)
#'
#' @name set-outer
NULL


#' @rdname set-outer
#' @export
set_outer_borders <- function (ht, row, col, value = 0.4) {
  assert_that(is_huxtable(ht))
  if (nargs() == 2) {
    if (missing(value)) value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  }
  rc <- outer_row_col_value(ht, row, col, value)
  row <- rc$row
  col <- rc$col
  value <- rc$value

  left_border(ht)[row, min(col)]    <- value
  right_border(ht)[row, max(col)]   <- value
  top_border(ht)[min(row), col]     <- value
  bottom_border(ht)[max(row), col]  <- value

  ht
}


#' @rdname set-outer
#' @export
set_outer_border_colors <- function (ht, row, col, value) {
  assert_that(is_huxtable(ht))
  rc <- outer_row_col_value(ht, row, col, value)
  row <- rc$row
  col <- rc$col
  value <- rc$value

  left_border_color(ht)[row, min(col)]    <- value
  right_border_color(ht)[row, max(col)]   <- value
  top_border_color(ht)[min(row), col]     <- value
  bottom_border_color(ht)[max(row), col]  <- value

  ht
}


#' @rdname set-outer
#' @export
set_outer_border_styles <- function (ht, row, col, value) {
  assert_that(is_huxtable(ht))
  rc <- outer_row_col_value(ht, row, col, value)
  row <- rc$row
  col <- rc$col
  value <- rc$value

  left_border_style(ht)[row, min(col)]    <- value
  right_border_style(ht)[row, max(col)]   <- value
  top_border_style(ht)[min(row), col]     <- value
  bottom_border_style(ht)[max(row), col]  <- value

  ht
}


#' @rdname set-outer
#' @export
set_outer_padding <- function (ht, row, col, value) {
  assert_that(is_huxtable(ht))
  rc <- outer_row_col_value(ht, row, col, value)
  row <- rc$row
  col <- rc$col
  value <- rc$value

  left_padding(ht)[row, min(col)]    <- value
  right_padding(ht)[row, max(col)]   <- value
  top_padding(ht)[min(row), col]     <- value
  bottom_padding(ht)[max(row), col]  <- value

  ht
}


outer_row_col_value <- function(ht, row, col, value) {
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  }

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  return(list(row = row, col = col, value = value))
}

