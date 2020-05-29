
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
  # new_brdr(
  #   thickness = x$thickness[...],
  #   style     = x$style[...],
  #   color     = x$color[...]
  # )
  x$thickness[...]
}


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


#' @export
print.brdr <- function (x, ...) cat(format(x))


is_brdr <- function(x) inherits(x, "brdr")


is_borderish <- function (x) {
  (is.numeric(x) && all(x >= 0)) || is_brdr(x)
}
