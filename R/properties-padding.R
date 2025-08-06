#' Set padding
#'
#' These functions set the space around the edges of cells, within the borders.
#'
#' @eval make_border_aliases("padding")
#'
#' @template property-params
#' @param value Numeric: padding width/height in points.

#' @seealso [set-multiple], [set-outer].
#'
#' @examples
#'
#' left_padding(jams) <- 2
#' left_padding(jams)
#'
#' jams <- set_left_padding(jams, 2)
#' left_padding(jams)
#'
#' @name padding
NULL

left_padding <- function(ht) .prop_get(ht, "left_padding")

`left_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "left_padding")
}

set_left_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "left_padding")
}

map_left_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "left_padding")
}

right_padding <- function(ht) .prop_get(ht, "right_padding")

`right_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "right_padding")
}

set_right_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "right_padding")
}

map_right_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "right_padding")
}

top_padding <- function(ht) .prop_get(ht, "top_padding")

`top_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "top_padding")
}

set_top_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "top_padding")
}

map_top_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "top_padding")
}

bottom_padding <- function(ht) .prop_get(ht, "bottom_padding")

`bottom_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "bottom_padding")
}

set_bottom_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "bottom_padding")
}

map_bottom_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "bottom_padding")
}


#' @name left_padding
#' @rdname padding
#' @templateVar attr_name left_padding
#' @template cell-property-usage
NULL

#' @name right_padding
#' @rdname padding
#' @templateVar attr_name right_padding
#' @template cell-property-usage
NULL


#' @name top_padding
#' @rdname padding
#' @templateVar attr_name top_padding
#' @template cell-property-usage
NULL


#' @name bottom_padding
#' @rdname padding
#' @templateVar attr_name bottom_padding
#' @template cell-property-usage
NULL
