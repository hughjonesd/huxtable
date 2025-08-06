

# helper functions -----------------------------------------------------------


.border_prop_set <- function (ht, row, col, value, side, prop,
        check_fun = NULL, check_values = NULL) {
  getter <- get(paste0(side, "_", prop))
  attr(ht, prop) <- getter(ht)
  extra <- substitute({
    FUN(ht)[rc$row, rc$col] <- value
  }, list(FUN = as.name(paste0(side, "_", prop))))
  if (prop == "border" && is_brdr(value)) {
    ht <- .prop_set(ht, row, col, value, prop,
          extra = extra, reset_na = FALSE)
  } else {
    ht <- .prop_set(ht, row, col, value, prop,
          check_fun = check_fun, check_values = check_values, extra = extra)
  }
  attr(ht, prop) <- NULL
  ht
}

.border_prop_map <- function (ht, row, col, fn, side, prop,
        check_fun = NULL, check_values = NULL) {
  getter <- get(paste0(side, "_", prop))
  attr(ht, prop) <- getter(ht)
  extra <- substitute({
    FUN(ht)[rc$row, rc$col] <- value
  }, list(FUN = as.name(paste0(side, "_", prop))))
  if (prop == "border") {
    ht <- .prop_map(ht, row, col, fn, prop,
          check_fun = is_borderish, extra = extra, reset_na = FALSE)
  } else {
    ht <- .prop_map(ht, row, col, fn, prop,
          check_fun = check_fun, check_values = check_values, extra = extra)
  }
  attr(ht, prop) <- NULL
  ht
}

.border_set <- function (ht, row, col, value, side,
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
        check_fun = is_borderish)
}

allowed_border_styles <- c("solid", "double", "dashed", "dotted")

# thickness -----------------------------------------------------------------

set_left_border <- function (ht, row, col, value = 0.4) {
  .border_set(ht, row, col, value, "left",
        missing(row), missing(col), missing(value))
}

map_left_border <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border")
}

set_right_border <- function (ht, row, col, value = 0.4) {
  .border_set(ht, row, col, value, "right",
        missing(row), missing(col), missing(value))
}

map_right_border <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border")
}

set_top_border <- function (ht, row, col, value = 0.4) {
  .border_set(ht, row, col, value, "top",
        missing(row), missing(col), missing(value))
}

map_top_border <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border")
}

set_bottom_border <- function (ht, row, col, value = 0.4) {
  .border_set(ht, row, col, value, "bottom",
        missing(row), missing(col), missing(value))
}

map_bottom_border <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border")
}

# color ---------------------------------------------------------------------

set_left_border_color <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "left", "border_color",
        check_fun = is.character)
}

map_left_border_color <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border_color",
        check_fun = is.character)
}

set_right_border_color <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "right", "border_color",
        check_fun = is.character)
}

map_right_border_color <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border_color",
        check_fun = is.character)
}

set_top_border_color <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "top", "border_color",
        check_fun = is.character)
}

map_top_border_color <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border_color",
        check_fun = is.character)
}

set_bottom_border_color <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "bottom", "border_color",
        check_fun = is.character)
}

map_bottom_border_color <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border_color",
        check_fun = is.character)
}

# style ---------------------------------------------------------------------

set_left_border_style <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "left", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

map_left_border_style <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "left", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

set_right_border_style <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "right", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

map_right_border_style <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "right", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

set_top_border_style <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "top", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

map_top_border_style <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "top", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

set_bottom_border_style <- function (ht, row, col, value) {
  .border_prop_set(ht, row, col, value, "bottom", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

map_bottom_border_style <- function (ht, row, col, fn) {
  .border_prop_map(ht, row, col, fn, "bottom", "border_style",
        check_fun = is.character, check_values = allowed_border_styles)
}

# order of these matters
border_props <- c(
  "top_border", "left_border", "right_border", "bottom_border",
  "top_border_color", "left_border_color", "right_border_color", "bottom_border_color",
  "top_border_style", "left_border_style", "right_border_style", "bottom_border_style"
)

#' @evalNamespace make_exports(border_props, with_map = TRUE)
NULL


#' Set borders
#'
#' These functions set borders between cells.
#'
#' @eval make_border_aliases("border")
#'
#' @template property-params
#' @param value A numeric thickness or a [brdr()] object.
#'
#' @details
#' Borders are always "collapsed": `right_border(ht)[, 1]` is
#' the same as `left_border(ht)[, 2]`, and setting one sets the other.
#'
#' Setting `left_border(ht) <- number` sets the border thickness.
#' You can set multiple properties at once by using [brdr()].
#'
#' Currently in LaTeX, all non-zero border widths on a given line must be the
#' same.
#'
#' @section Limitations:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX, and
#'   all non-zero horizontal border widths on a given line must be the same.
#'
#'
#' @seealso [set-multiple]
#'
#' @family border properties
#'
#' @examples
#'
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


#' @name left_border
#' @rdname borders
#' @templateVar attr_name left_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name right_border
#' @rdname borders
#' @templateVar attr_name right_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name top_border
#' @rdname borders
#' @templateVar attr_name top_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name bottom_border
#' @rdname borders
#' @templateVar attr_name bottom_border
#' @templateVar default 0.4
#' @template cell-property-usage
NULL


#' @name `left_border<-`
#' @rdname borders
NULL


#' @name `right_border<-`
#' @rdname borders
NULL


#' @name `top_border<-`
#' @rdname borders
NULL


#' @name `bottom_border<-`
#' @rdname borders
NULL


#' Set border colors
#'
#' These functions set border colors.
#'
#' @eval make_border_aliases("border_color")
#'
#' @template property-params
#' @param value A valid R color, e.g. `"red"`, `"#FF0000"`.
#'
#' @details
#' Borders are always "collapsed": `right_border_color(ht)[, 1]` is
#' the same as `left_border_color(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#'
#' * Transparent borders with the alpha channel set are not guaranteed to work.
#'
#' @seealso [set-multiple], [brdr()]
#'
#' @family border properties
#'
#' @examples
#'
#' jams <- set_all_borders(jams)
#' bottom_border_color(jams)[1, ] <- "red"
#' jams
#'
#' set_bottom_border_color(jams, "blue")
#'
#' @name border-colors
NULL


#' @name left_border_color
#' @rdname border-colors
#' @templateVar attr_name left_border_color
#' @template cell-property-usage
NULL


#' @name right_border_color
#' @rdname border-colors
#' @templateVar attr_name right_border_color
#' @template cell-property-usage
NULL


#' @name top_border_color
#' @rdname border-colors
#' @templateVar attr_name top_border_color
#' @template cell-property-usage
NULL


#' @name bottom_border_color
#' @rdname border-colors
#' @templateVar attr_name bottom_border_color
#' @template cell-property-usage
NULL


#' @name `left_border_color<-`
#' @rdname border-colors
NULL


#' @name `right_border_color<-`
#' @rdname border-colors
NULL


#' @name `top_border_color<-`
#' @rdname border-colors
NULL


#' @name `bottom_border_color<-`
#' @rdname border-colors
NULL



#' Set border styles
#'
#' These functions set border styles.
#'
#' @eval make_border_aliases("border_style")
#'
#' @template property-params
#' @param value One of `"solid"`, `"double"`, `"dashed"` or `"dotted"`.
#'
#' @details
#' Borders are always "collapsed": `right_border_style(ht)[, 1]` is
#' the same as `left_border_style(ht)[, 2]`, and setting one sets the other.
#'
#' @section Limitations:
#'
#' * In HTML, you will need to set a width of at least 3 to get a double border.
#' * Only "solid" and "double" styles are currently implemented in LaTeX.
#'
#' @seealso [set-multiple], [brdr()]
#'
#' @family border properties
#'
#' @examples
#'
#' jams <- set_all_borders(jams)
#' bottom_border_style(jams)[1, ] <- "dotted"
#' jams
#'
#' set_bottom_border_style(jams, "double")
#'
#' @name border-styles
NULL


#' @name left_border_style
#' @rdname border-styles
#' @templateVar attr_name left_border_style
#' @template cell-property-usage
NULL


#' @name right_border_style
#' @rdname border-styles
#' @templateVar attr_name right_border_style
#' @template cell-property-usage
NULL


#' @name top_border_style
#' @rdname border-styles
#' @templateVar attr_name top_border_style
#' @template cell-property-usage
NULL


#' @name bottom_border_style
#' @rdname border-styles
#' @templateVar attr_name bottom_border_style
#' @template cell-property-usage
NULL


#' @name `left_border_style<-`
#' @rdname border-styles
NULL


#' @name `right_border_style<-`
#' @rdname border-styles
NULL


#' @name `top_border_style<-`
#' @rdname border-styles
NULL


#' @name `bottom_border_style<-`
#' @rdname border-styles
NULL
