
for (val in paste0(c("left", "right", "top", "bottom"), "_border")) {
  set_fun <- paste0("set_", val)
  map_fun <- paste0("map_", val)
  attr_fun <- as.name(val)
  assign(set_fun, eval(bquote(
    function (ht, row, col, value = 0.4) {
      assert_that(is_huxtable(ht))
      if (missing(row) && missing(col) && missing(value)) {
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else if (missing(col) && missing(value)) {
        value <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      .(attr_fun)(ht)[rc$row, rc$col] <- value
      ht
    }
  )))
  assign(map_fun, eval(bquote(
    function (ht, row, col, fn) {
      assert_that(is_huxtable(ht))
      if (missing(col) && missing(fn)) {
        fn <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      current <- .(attr_fun)(ht)[rc$row, rc$col, drop = FALSE]
      if (is_huxtable(current)) current <- as.matrix(current)
      .(attr_fun)(ht)[rc$row, rc$col] <- fn(ht, rc$row, rc$col, current)
      ht
    }
  )))
}

for (val in paste0(c("left", "right", "top", "bottom"), "_border_color")) {
  set_fun <- paste0("set_", val)
  map_fun <- paste0("map_", val)
  attr_fun <- as.name(val)
  assign(set_fun, eval(bquote(
    function (ht, row, col, value) {
      assert_that(is_huxtable(ht))
      if (missing(row) && missing(col) && missing(value)) {
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else if (missing(col) && missing(value)) {
        value <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      .(attr_fun)(ht)[rc$row, rc$col] <- value
      ht
    }
  )))
  assign(map_fun, eval(bquote(
    function (ht, row, col, fn) {
      assert_that(is_huxtable(ht))
      if (missing(col) && missing(fn)) {
        fn <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      current <- .(attr_fun)(ht)[rc$row, rc$col, drop = FALSE]
      if (is_huxtable(current)) current <- as.matrix(current)
      .(attr_fun)(ht)[rc$row, rc$col] <- fn(ht, rc$row, rc$col, current)
      ht
    }
  )))
}

for (val in paste0(c("left", "right", "top", "bottom"), "_border_style")) {
  set_fun <- paste0("set_", val)
  map_fun <- paste0("map_", val)
  attr_fun <- as.name(val)
  assign(set_fun, eval(bquote(
    function (ht, row, col, value) {
      assert_that(is_huxtable(ht))
      if (missing(row) && missing(col) && missing(value)) {
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else if (missing(col) && missing(value)) {
        value <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      .(attr_fun)(ht)[rc$row, rc$col] <- value
      ht
    }
  )))
  assign(map_fun, eval(bquote(
    function (ht, row, col, fn) {
      assert_that(is_huxtable(ht))
      if (missing(col) && missing(fn)) {
        fn <- row
        row <- seq_len(nrow(ht))
        col <- seq_len(ncol(ht))
      } else {
        if (missing(row)) row <- seq_len(nrow(ht))
        if (missing(col)) col <- seq_len(ncol(ht))
      }
      rc <- list()
      rc$row <- get_rc_spec(ht, row, 1)
      rc$col <- get_rc_spec(ht, col, 2)
      current <- .(attr_fun)(ht)[rc$row, rc$col, drop = FALSE]
      if (is_huxtable(current)) current <- as.matrix(current)
      .(attr_fun)(ht)[rc$row, rc$col] <- fn(ht, rc$row, rc$col, current)
      ht
    }
  )))
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
