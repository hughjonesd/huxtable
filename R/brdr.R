
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
`[.brdr` <- function (x, ...) {
  new_brdr(
    thickness = x$thickness[...],
    style     = x$style[...],
    color     = x$color[...]
  )
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
#' \method{[}{bdr}(x, ...) <- value
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


#' @export
#' @method `[<-.brdr` brdr
`[<-.brdr.brdr` <- function(x, ..., value) {
  x$thickness[...] <- value$thickness
  x$style[...]     <- value$style
  x$color[...]     <- value$color

  x
}


#' @export
#' @method `[<-.brdr` default
`[<-.brdr.default` <- function(x, ..., value) {
  x$thickness[...] <- value

  x
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


#' @export
print.brdr <- function (x, ...) cat(format(x))


is_brdr <- function(x) inherits(x, "brdr")


is_borderish <- function (x) {
  (is.numeric(x) && all(x >= 0)) || is_brdr(x)
}
