
# Convenience getter/setters for multiple properties at once ---------------------------------------

#' @import assertthat
NULL


#' Default huxtable properties
#'
#' Defaults are used for new huxtables, and also when a property is set to `NA`.
#'
#' @param ... Properties specified by name, or a single named list.
#'
#' @return For `set_default_properties`, a list of the previous property values, invisibly.
#' @details
#' Note that `autoformat = TRUE` in [huxtable()] overrides some defaults.
#' @export
#' @seealso Options for autoformat in [huxtable-options].
#' @examples
#' old <- set_default_properties(left_border = 1)
#' hux(a = 1:2, b = 1:2)
#' set_default_properties(old)
set_default_properties <- function(...) {
  defaults <- list(...)
  if (is.list(defaults[[1]]) && is.null(names(defaults))) defaults <- defaults[[1]]

  if (length(unrec <- setdiff(names(defaults), names(huxtable_env$huxtable_default_attrs))) > 0)
    stop('Unrecognized huxtable property name(s): ', paste(unrec, collapse = ', '),
      '; to see all names, use get_default_properties()')
  old <- huxtable_env$huxtable_default_attrs[names(defaults)]
  huxtable_env$huxtable_default_attrs[names(defaults)] <- defaults

  invisible(old)
}


#' Get default huxtable properties
#'
#' @param names Vector of property names. If `NULL`, all properties are returned.
#'
#' @return For `get_default_properties`, a list of the current defaults.
#' @export
#'
#' @examples
#' get_default_properties('bold')
#' @rdname set_default_properties
get_default_properties <- function (names = NULL) {
  names <- names %||% names(huxtable_env$huxtable_default_attrs)
  if (length(unrec <- setdiff(names, names(huxtable_env$huxtable_default_attrs))) > 0) stop(
    'Unrecognized property name(s): ', paste(unrec, collapse = ', '),
    '; to see all names, use get_default_properties()')
  huxtable_env$huxtable_default_attrs[names]
}


#' Set left, right, top and bottom properties
#'
#' These are convenience functions which set left, right, top and bottom properties
#' simultaneously for the specified cells.
#'
#' @param ht A huxtable.
#' @param value Value(s) to set. Set to `NA` to reset to the default.
#' @param row A row specifier. See [rowspecs].
#' @param col An optional column specifier.
#' @param byrow If \code{TRUE}, fill in values by row rather than by column.
#' @param fn A mapping function. See [mapping-functions].
#'
#' @return The modified huxtable.
#' @name set-multiple
NULL


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' set_all_borders(ht, 1:3, 1:2, 1)
set_all_borders <- function(ht, row, col, value, byrow = FALSE) {
  recall_ltrb(ht, 'set_%s_border')
}

#' @rdname set-multiple
#' @export
map_all_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, 'map_%s_border')
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_colors(ht, 'red')
set_all_border_colors <- function(ht, row, col, value, byrow = FALSE) {
  recall_ltrb(ht, 'set_%s_border_color')
}

#' @rdname set-multiple
#' @export
map_all_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, 'map_%s_border_color')
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_styles(ht, 'double')
set_all_border_styles <- function(ht, row, col, value, byrow = FALSE) {
  recall_ltrb(ht, 'set_%s_border_style')
}

#' @rdname set-multiple
#' @export
map_all_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, 'map_%s_border_style')
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_padding(ht, 1:3, 1:2, "20px")
set_all_padding <- function(ht, row, col, value, byrow = FALSE) {
  recall_ltrb(ht, 'set_%s_padding')
}


#' @rdname set-multiple
#' @export
map_all_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, 'map_%s_padding')
}


#' @rdname set-multiple
#' @details
#' `set_outer_borders` sets borders round the top, bottom, left and right of a group
#' of cells. Behaviour is undefined unless `row` and `col` specify contiguous sequences.
#'
#' @export
#' @examples
#' ht2 <- huxtable(a = 1:3, b = 1:3)
#' set_outer_borders(ht2, 1)
#' set_outer_borders(ht2, 2:3, 1:2, 1)
#'
#' # Problems with colspan:
#' rowspan(ht2)[2, 1] <- 2
#' set_outer_borders(ht2, 1:2, 1:2, 1)
#'
set_outer_borders <- function(ht, row, col, value) {
  assert_that(is_huxtable(ht))

  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else if (missing(value)) {
    value <- col
    if (!is.matrix(row)) stop("No columns specified, but `row` argument did not evaluate to a matrix")
    # row is a 2-matrix of row, col vectors;
    col <- seq(min(row[, 2]), max(row[, 2]))
    row <- seq(min(row[, 1]), max(row[, 1]))
  }
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  left_border(ht)[row, min(col)]    <- value
  right_border(ht)[row, max(col)]   <- value
  top_border(ht)[min(row), col]     <- value
  bottom_border(ht)[max(row), col]  <- value

  ht
}


#' Set multiple cell properties
#'
#' @param ht A huxtable.
#' @param row A row specification.
#' @param col A column specification.
#' @param ... Named list of cell properties.
#'
#' @return The modified huxtable object.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:3, b = 1:3)
#' ht <- set_cell_properties(ht, 1, 1, italic = TRUE, text_color = 'red')
#' text_color(ht)
#' ht
set_cell_properties <- function (ht, row, col, ...) {
  props <- list(...)
  if (! all(names(props) %in% huxtable_cell_attrs)) stop('Unrecognized properties: ', paste(setdiff(names(props),
    huxtable_cell_attrs), collapse = ', '))
  call <- match.call(expand.dots = FALSE)
  call[['...']] <- NULL
  for (prop_name in names(props)) {
    call[[1]] <- as.symbol(paste0('set_', prop_name))
    call$value <- props[[prop_name]]
    ht <- eval(call, list(ht = ht), parent.frame())
  }

  ht
}


#' Merge a range of cells
#'
#' @param ht A huxtable.
#' @param row A row specifier. See \code{\link{rowspecs}} for details. Only the minimum and maximum
#' rows and columns are used.
#' @param col A column specifier.
#'
#' @details
#' `merge_cells(ht, c(min_row, max_row), c(min_col, max_col))` is equivalent to
#' ```
#'   colspan(ht)[min_row, min_col] <- max_col - min_col + 1
#'   rowspan(ht)[min_row, min_col] <- max_row - min_row + 1
#' ```
#' @return The `ht` object.
#'
#' @export
#' @examples
#' ht <- hux(a = 1:3, b = 1:3)
#' ht <- set_all_borders(ht, 1)
#' merge_cells(ht, 1:2, 1:2)
merge_cells <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  if (missing(col)) {
    .Deprecated('Using merge_cells without a `col` argument is deprecated.', package = 'huxtable')
    if (! is.matrix(row)) stop(
      'No columns specified, but `row` argument did not evaluate to a matrix')
    # 2-matrix of row, col vectors
    col <- seq(min(row[, 2]), max(row[, 2]))
    row <- seq(min(row[, 1]), max(row[, 1]))
  }
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  mr <- min(row)
  mc <- min(col)
  cs <- diff(range(col)) + 1
  rs <- diff(range(row)) + 1
  colspan(ht)[mr, mc] <- cs
  rowspan(ht)[mr, mc] <- rs

  ht
}


get_all_borders <- function(ht, row, col, drop = TRUE) {
  list(
    left   = left_border(ht)[row, col, drop = drop],
    right  = right_border(ht)[row, col, drop = drop],
    top    = top_border(ht)[row, col, drop = drop],
    bottom = bottom_border(ht)[row, col, drop = drop]
  )
}


get_all_border_colors <- function(ht, row, col, drop = TRUE) {
  list(
    left   = left_border_color(ht)[row, col, drop = drop],
    right  = right_border_color(ht)[row, col, drop = drop],
    top    = top_border_color(ht)[row, col, drop = drop],
    bottom = bottom_border_color(ht)[row, col, drop = drop]
  )
}


get_all_border_styles <- function(ht, row, col, drop = TRUE) {
  list(
    left   = left_border_style(ht)[row, col, drop = drop],
    right  = right_border_style(ht)[row, col, drop = drop],
    top    = top_border_style(ht)[row, col, drop = drop],
    bottom = bottom_border_style(ht)[row, col, drop = drop]
  )
}


get_all_padding <- function(ht, row, col, drop = TRUE) {
  list(
    left   = left_padding(ht)[row, col, drop = drop],
    right  = right_padding(ht)[row, col, drop = drop],
    top    = top_padding(ht)[row, col, drop = drop],
    bottom = bottom_padding(ht)[row, col, drop = drop]
  )
}
