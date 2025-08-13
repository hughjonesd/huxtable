# helper functions -----------------------------------------------------------


.border_prop_set <- function(ht, row, col, value, side, prop,
                             check_fun = NULL, check_values = NULL) {
  # Handle two-argument form: set_*_border_*(ht, value)
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  getter <- get(paste0(side, "_", prop))
  attr(ht, prop) <- getter(ht)
  extra <- substitute(
    {
      FUN(ht)[rc$row, rc$col] <- value
    },
    list(FUN = as.name(paste0(side, "_", prop)))
  )
  if (prop == "border" && is_brdr(value)) {
    ht <- prop_set(ht, prop, row, col, value = value,
      extra = extra, reset_na = FALSE
    )
  } else {
    ht <- prop_set(ht, prop, row, col, value = value,
      check_fun = check_fun, check_values = check_values, extra = extra
    )
  }
  attr(ht, prop) <- NULL
  ht
}

.border_prop_map <- function(ht, row, col, fn, side, prop,
                             check_fun = NULL, check_values = NULL) {
  # Handle two-argument form: map_*_border_*(ht, fn)
  if (missing(col) && missing(fn)) {
    fn <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  getter <- get(paste0(side, "_", prop))
  attr(ht, prop) <- getter(ht)
  extra <- substitute(
    {
      FUN(ht)[rc$row, rc$col] <- value
    },
    list(FUN = as.name(paste0(side, "_", prop)))
  )
  if (prop == "border") {
    ht <- prop_set(ht, prop, row, col, fn = fn,
      check_fun = is_borderish, extra = extra, reset_na = FALSE
    )
  } else {
    ht <- prop_set(ht, prop, row, col, fn = fn,
      check_fun = check_fun, check_values = check_values, extra = extra
    )
  }
  attr(ht, prop) <- NULL
  ht
}

.border_set <- function(ht, row, col, value, side,
                        missing_row, missing_col, missing_value) {
  if (missing_row && missing_col && missing_value) {
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
    value <- 0.4
  } else if (missing_col && missing_value) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing_row) row <- seq_len(nrow(ht))
    if (missing_col) col <- seq_len(ncol(ht))
    if (missing_value) value <- 0.4
  }
  .border_prop_set(ht, row, col, value, side, "border",
    check_fun = is_borderish
  )
}

allowed_border_styles <- c("solid", "double", "dashed", "dotted")

# thickness -----------------------------------------------------------------

#' Set borders
#'
#' These functions set borders between cells.
#'
#' @inheritParams hux_prop_params
#' @param value A numeric thickness or a [brdr()] object. `r rd_default("border")`
#'
#' @details
#' Borders are always "collapsed": `right_border(ht)[, 1]` is the same as
#' `left_border(ht)[, 2]`, and setting one sets the other.
#'
#' Setting `left_border(ht) <- number` sets the border thickness. You can set
#' multiple properties at once by using [brdr()].
#'
#' Currently in LaTeX, all non-zero border widths on a given line must be the
#' same.
#'
#' @section Limitations:
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX, and
#'   all non-zero horizontal border widths on a given line must be the same.
#'
#' @seealso [set-multiple]
#' @family border properties
#' @examples
#' bottom_border(jams)[1, ] <- 0.4
#' jams
#'
#' bottom_border(jams)[1, ] <- brdr(0.4, "solid", "blue")
#' jams
#'
#' set_bottom_border(jams, brdr(0.4, "solid", "green"))
#'
#' @name borders
NULL

#' @rdname borders
#' @export
set_left_border <- function(ht, row, col, value = 0.4) {
  .border_set(
    ht, row, col, value, "left",
    missing(row), missing(col), missing(value)
  )
}

#' @rdname borders
#' @export
set_right_border <- function(ht, row, col, value = 0.4) {
  .border_set(
    ht, row, col, value, "right",
    missing(row), missing(col), missing(value)
  )
}

#' @rdname borders
#' @export
set_top_border <- function(ht, row, col, value = 0.4) {
  .border_set(
    ht, row, col, value, "top",
    missing(row), missing(col), missing(value)
  )
}

#' @rdname borders
#' @export
set_bottom_border <- function(ht, row, col, value = 0.4) {
  .border_set(
    ht, row, col, value, "bottom",
    missing(row), missing(col), missing(value)
  )
}

#' @rdname borders
#' @export
map_left_border <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border")
}

#' @rdname borders
#' @export
map_right_border <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border")
}

#' @rdname borders
#' @export
map_top_border <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border")
}

#' @rdname borders
#' @export
map_bottom_border <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border")
}

# color ---------------------------------------------------------------------

#' Set border colors
#'
#' These functions set border colors.
#'
#' @inheritParams borders
#' @param value A valid R color, e.g. `"red"`, `"#FF0000"`. `r rd_default("border_color")`
#'
#' @details
#' Borders are always "collapsed": `right_border_color(ht)[, 1]` is the same as
#' `left_border_color(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#' * Transparent borders with the alpha channel set are not guaranteed to work.
#'
#' @seealso [set-multiple], [brdr()]
#' @family border properties
#' @examples
#' jams <- set_all_borders(jams)
#' bottom_border_color(jams)[1, ] <- "red"
#' jams
#'
#' set_bottom_border_color(jams, "blue")
#'
#' @name border-colors
NULL

#' @rdname border-colors
#' @export
set_left_border_color <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "left", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
set_right_border_color <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "right", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
set_top_border_color <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "top", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
set_bottom_border_color <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "bottom", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
map_left_border_color <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
map_right_border_color <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
map_top_border_color <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border_color",
    check_fun = is.character
  )
}

#' @rdname border-colors
#' @export
map_bottom_border_color <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border_color",
    check_fun = is.character
  )
}

# style ---------------------------------------------------------------------

#' Set border styles
#'
#' These functions set border styles.
#'
#' @inheritParams borders
#' @param value One of `"solid"`, `"double"`, `"dashed"` or `"dotted"`. `r rd_default("border_style")`
#'
#' @details
#' Borders are always "collapsed": `right_border_style(ht)[, 1]` is the same as
#' `left_border_style(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX.
#'
#' @seealso [set-multiple], [brdr()]
#' @family border properties
#' @examples
#' jams <- set_all_borders(jams)
#' bottom_border_style(jams)[1, ] <- "dotted"
#' jams
#'
#' set_bottom_border_style(jams, "double")
#'
#' @name border-styles
NULL

#' @rdname border-styles
#' @export
set_left_border_style <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "left", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
set_right_border_style <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "right", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
set_top_border_style <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "top", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
set_bottom_border_style <- function(ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "bottom", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
map_left_border_style <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
map_right_border_style <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
map_top_border_style <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}

#' @rdname border-styles
#' @export
map_bottom_border_style <- function(ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border_style",
    check_fun = is.character, check_values = allowed_border_styles
  )
}
