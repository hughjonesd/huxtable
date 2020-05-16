
#' Create a border object
#'
#' @param thickness Thickness of the border in points
#' @param style "solid" (the default), "double", "dashed" or "dotted".
#' @param color String representing a valid color (either a color name or
#'   a hexadecimalstring like "#00FF00").
#'
#' @return An object of class "bdr" which you can pass into huxtable
#'   border functions
#' @export
#'
#' @examples
#'
#' set_bottom_border(jams, bdr(1, "solid", "red"))
#'
bdr <- function (thickness = 0.4, style = "solid", color = NA_character_) {
  assert_that(is.number(thickness), thickness >= 0, is.string(style),
    style %in% c("solid", "double", "dashed", "dotted"), is.string(color))
  new_bdr(thickness = thickness, style = style, color = color)
}


new_bdr <- function (thickness, style, color) {
  structure(
    list(thickness = thickness, style = style, color = color),
    class = "bdr"
  )
}


#' @export
format.bdr <- function(x, ...) {
  glue::glue("Border: thickness {x$thickness}, style {x$style}, color {x$color}")
}


#' @export
print.bdr <- function (x, ...) cat(format(x))
