
#' Create a border object
#'
#' @param thickness Thickness of the border in points
#' @param style "solid" (the default), "double", "dashed" or "dotted".
#' @param color String representing a valid color (either a color name or
#'   a hexadecimalstring like "#00FF00").
#'
#' @return An object of class "brdr" which you can pass into huxtable
#'   border functions
#' @export
#'
#' @examples
#'
#' set_bottom_border(jams, brdr(1, "solid", "red"))
#'
brdr <- function (thickness = 0.4, style = "solid", color = NA_character_) {
  assert_that(is.number(thickness), thickness >= 0, is.string(style),
    style %in% c("solid", "double", "dashed", "dotted"), is.string(color))
  new_brdr(thickness = thickness, style = style, color = color)
}


#' Replace a subset of a brdr object
#'
#' You probably don't need to call this directly. If you want
#' to access border thicknesses, do e.g.
#'
#' ```r
#' l_borders <- brdr_thickness(left_border(ht))
#' ```
#'
#' which will give you a matrix of numbers.
#'
#' @usage
#' \method{[}{brdr}(x, ...) <- value
#'
#' @param x A `brdr` object.
#' @param ... Indices.
#' @param value A [brdr()] object, number or matrix.
#'
#' @return A [brdr()] object.
#'
#' @export
#' @method `[<-` brdr
#' @export `[<-.brdr`
`[<-.brdr` <- function (x, ..., value) {
  UseMethod("[<-.brdr", value)
}


#' Get thickness of a [brdr()] object.
#'
#' @param x A [brdr()] object.
#'
#' @return A number or numeric matrix.
#'
#' @export
#'
#' @examples
#'
#' brdr_thickness(left_border(jams))
#' brdr_thickness(brdr(1, "solid", "red"))
#'
brdr_thickness <- function (x) {
  x$thickness
}


new_brdr <- function (thickness, style, color) {
  structure(
    list(thickness = thickness, style = style, color = color),
    class = "brdr"
  )
}


#' @export
format.brdr <- function(x, ...) {
  glue::glue("Border: thickness {x$thickness}, style {x$style}, color {x$color}")
}


#' @export
print.brdr <- function (x, ...) cat(format(x))


is_brdr <- function(x) inherits(x, "brdr")


is_borderish <- function (x) {
  (is.numeric(x) && all(x >= 0)) || is_brdr(x)
}


#' @export
#' @method `[<-.brdr` brdr
`[<-.brdr.brdr` <- function(x, ..., value) {
  x$thickness[...] <- value$thickness
  x$style[...]     <- value$style
  x$color[...]     <- value$color

  x
}


#' @export
#' @method `[<-.brdr` list
`[<-.brdr.list` <- function (x, ..., value) {
  # we assume each element is a brdr object of length 1 and then apply them to
  # each element in turn.`...` is probably a logical vector with no
  # indices, so we can't just concatenate the values.
  assert_that(all(sapply(value, is_brdr)))
  thickness <- vapply(value, function (y) y$thickness, FUN.VALUE = numeric(1))
  style <- vapply(value, function (y) y$style, FUN.VALUE = character(1))
  color <- vapply(value, function (y) y$color, FUN.VALUE = character(1))

  x$thickness[...] <- thickness
  x$style[...]     <- style
  x$color[...]     <- color

  x
}


#' @export
#' @method `[<-.brdr` default
`[<-.brdr.default` <- function(x, ..., value) {
  x$thickness[...] <- value

  x
}


#' @export
`[.brdr` <- function (x, ...) {
  thickness <- x$thickness[...]
  style     <- x$style[...]
  color     <- x$color[...]
  new_brdr(
    thickness = thickness,
    style     = style,
    color     = color
  )
}


#' @export
t.brdr <- function (x) {
  new_brdr(
    thickness = t(x$thickness),
    style     = t(x$style),
    color     = t(x$color)
  )
}


#' @export
dim.brdr <- function (x) {
  dim(x$thickness)
}


#' @export
`dimnames<-.brdr` <- function (x, value) {
  dimnames(x$thickness) <- value
  dimnames(x$style) <- value
  dimnames(x$color) <- value

  x
}


#' @export
rbind.brdr <- function (...) {
  b1 <- ..1
  b2 <- ..2
  new_brdr(
    thickness = rbind(b1$thickness, b2$thickness),
    style     = rbind(b1$style, b2$style),
    color     = rbind(b1$color, b2$color)
  )
}


#' @export
cbind.brdr <- function (...) {
  b1 <- ..1
  b2 <- ..2
  new_brdr(
    thickness = cbind(b1$thickness, b2$thickness),
    style     = cbind(b1$style, b2$style),
    color     = cbind(b1$color, b2$color)
  )
}


#' @export
is.na.brdr <- function (x) {
  is.na(x$thickness)
}
