
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
  check_recognized_properties(names(defaults))

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
#' get_default_properties("bold")
#' @rdname set_default_properties
get_default_properties <- function (names = NULL) {
  names <- names %||% names(huxtable_env$huxtable_default_attrs)
  check_recognized_properties(names)

  huxtable_env$huxtable_default_attrs[names]
}


check_recognized_properties <- function (names) {
  if (length(unrec <- setdiff(names, names(huxtable_env$huxtable_default_attrs))) > 0) stop(
    "Unrecognized property name(s): ", paste(unrec, collapse = ", "),
    "; to see all names, use get_default_properties()")
}


#' Set properties on headers
#'
#' These functions set arbitrary cell properties on cells in header rows
#' and/or columns.
#'
#' @inherit left_border params return
#' @inherit set_cell_properties params
#'
#' @export
#'
#' @examples
#'
#' style_headers(jams, text_color = "red")
#' jams <- set_header_cols(jams, 1, TRUE)
#' style_header_cols(jams,
#'   text_color = c(NA, "red",
#'     "darkred", "purple")
#'   )
#'
style_headers <- function (ht, ...) {
  ht <- style_header_rows(ht, ...)
  ht <- style_header_cols(ht, ...)

  ht
}


#' @export
#' @rdname style_headers
style_header_rows <- function (ht, ...) {
  set_cell_properties(ht, header_rows(ht), everywhere, ...)
}


#' @export
#' @rdname style_headers
style_header_cols <- function (ht, ...) {
  set_cell_properties(ht, everywhere, header_cols(ht),  ...)
}


#' Set left, right, top and bottom properties
#'
#' These functions set left, right,
#' top and/or bottom properties
#' simultaneously for the specified cells.
#'
#' * `set_all_*` functions set top, bottom, left and right properties.
#' * `set_tb_*` functions set top and bottom properties.
#' * `set_lr_*` functions set left and right properties.
#'
#'
#' @inherit left_border params
#' @param value Value(s) to set. Set to `NA` to reset to the default.
#'
#' @return The modified huxtable.
#' @name set-multiple
#' @aliases set_multiple
NULL


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- as_hux(jams)
#' ht <- set_all_borders(ht)
#' ht
set_all_borders <- function(ht, row, col, value = 0.4) {
  recall_ltrb(ht, "set_%s_border")
}

#' @rdname set-multiple
#' @export
map_all_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_colors(ht, "red")
#' ht
set_all_border_colors <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color")
}

#' @rdname set-multiple
#' @export
map_all_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_border_styles(ht, "double")
set_all_border_styles <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style")
}

#' @rdname set-multiple
#' @export
map_all_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_all_padding(ht, 1:3, 1:2, "20px")
set_all_padding <- function(ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding")
}


#' @rdname set-multiple
#' @export
map_all_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding")
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_tb_padding(ht, 10)
set_tb_padding <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_padding <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_padding", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_padding <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_padding", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' ht <- set_tb_borders(ht)
set_tb_borders <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_borders <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_borders <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' set_tb_border_colors(ht, "red")
set_tb_border_colors <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_border_colors <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_color", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_border_colors <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_color", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
#' @examples
#' set_tb_border_styles(ht, "red")
set_tb_border_styles <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
map_tb_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style", sides = c("top", "bottom"))
}


#' @rdname set-multiple
#' @export
set_lr_border_styles <- function (ht, row, col, value) {
  recall_ltrb(ht, "set_%s_border_style", sides = c("left", "right"))
}


#' @rdname set-multiple
#' @export
map_lr_border_styles <- function (ht, row, col, fn) {
  recall_ltrb(ht, "map_%s_border_style", sides = c("left", "right"))
}


recall_ltrb <- function(ht, template,
      sides = c("left", "top", "right", "bottom")) {
  call <- sys.call(sys.parent(1L))
  call_names <- parse(text = paste0("huxtable::",
    sprintf(template, sides)))
  for (cn in call_names) {
    call[[1]] <- cn
    call[[2]] <- quote(ht)
    ht <- eval(call, list(ht = ht), parent.frame(2L)) # = sys.frame(sys.parent(1)) i.e. caller of orig
  }

  ht
}


#' Set borders around a rectangle of cells
#'
#' @inherit left_border params
#' @param value Border width in points, border color, or border style (see [left_border_style()]).
#' @details
#' `set_outer_borders` sets borders round the top, bottom, left and right of a group
#' of cells. Behaviour is undefined unless `row` and `col` specify contiguous sequences.
#' `set_outer_border_colors` and `set_outer_border_styles` set border colors and styles.
#'
#' @export
#' @examples
#' ht2 <- huxtable(a = 1:3, b = 1:3)
#' set_outer_borders(ht2)
#' set_outer_borders(ht2, 2:3, 1:2)
#'
#' # Problems with colspan:
#' rowspan(ht2)[2, 1] <- 2
#' set_outer_borders(ht2, 1:2, 1:2)
#'
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


#' @rdname set_outer_borders
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


#' @rdname set_outer_borders
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
#' ht <- set_cell_properties(ht, 1, 1,
#'       italic = TRUE, text_color = "red")
#' text_color(ht)
#' ht
set_cell_properties <- function (ht, row, col, ...) {
  props <- list(...)
  if (! all(names(props) %in% huxtable_cell_attrs)) stop("Unrecognized properties: ", paste(setdiff(names(props),
    huxtable_cell_attrs), collapse = ", "))
  call <- match.call(expand.dots = FALSE)
  call[["..."]] <- NULL
  call[["ht"]] <- quote(ht)
  for (prop_name in names(props)) {
    call[[1]] <- as.symbol(paste0("set_", prop_name))
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
#' @family cell merging
#'
#' @export
#' @examples
#'
#' ht <- hux(a = 1:3, b = 1:3)
#' ht <- set_all_borders(ht, 1)
#' merge_cells(ht, 2:3, 1:2)
#'
merge_cells <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

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


#' Merge cells across rows or down columns
#'
#' `merge_across` creates multicolumn cells within each row. `merge_down` creates
#' multirow cells within each column.
#'
#' @inherit left_border params
#'
#' @return The `ht` object.
#'
#' @export
#'
#' @family cell merging
#'
#' @examples
#'
#' ht <- as_hux(matrix(1:12, 4, 3, byrow = TRUE))
#' ht <- set_all_borders(ht, 1)
#' merge_across(ht, 2:4, 2:3)
#' merge_down(ht, 2:4, 2:3)
#'
merge_across <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  for (r in row) ht <- merge_cells(ht, r, col)

  ht
}


#' @rdname merge_across
#' @export
merge_down <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  for (cl in col) ht <- merge_cells(ht, row, cl)

  ht
}


#' Merge repeated rows into multirow cells
#'
#' @param ht A huxtable.
#' @param row A row specification.
#' @param col A column specification.
#'
#' @return The modified huxtable.
#' @export
#'
#' @details
#' Repeated rows in each column are merged into cells with
#' `rowspan > 1`.
#'
#' If `row` contains gaps, results may be unexpected (and a warning is given).
#'
#' @seealso merge_cells
#'
#' @examples
#' ht <- as_hux(jams[c(1, 2, 2, 3, 3, 4), ])
#' ht <- add_columns(ht, c("Sugar", "30%", "40%", "30%", "40%", "30%"),
#'       after = 1)
#' ht
#' merge_repeated_rows(ht)
#' merge_repeated_rows(ht, everywhere, "Type")
merge_repeated_rows <- function (ht, row, col) {
  assert_that(is_huxtable(ht))
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  if (length(row) > 1L && ! all(row == seq(min(row), max(row)))) {
    warning("Non-contiguous rows: ", paste(row, collapse = ", "))
  }
  for (cc in col) {
    contents <- ht[row, ][[cc]] # gets a vector
    new <- which(c(TRUE, contents[seq_len(length(contents) - 1)] != contents[-1]))
    spans <- diff(c(new, length(contents) + 1))
    rowspan(ht)[row[new], cc] <- spans
  }

  ht
}


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
