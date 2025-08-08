#' Cell padding
#'
#' Functions to get or set the space around cell borders. Top, bottom, left and
#' right padding all default to 6 points.
#'
#' @inheritParams hux_prop_params
#' @param value Numeric: padding width/height in points. `r rd_default("left_padding")`
#' @seealso [set-multiple], [set-outer].
#' @examples
#' left_padding(jams) <- 2
#' left_padding(jams)
#'
#' jams <- set_left_padding(jams, 2)
#' left_padding(jams)
#'
#' @name padding
NULL

#' @rdname padding
#' @export
left_padding <- function(ht) prop_get(ht, "left_padding")

#' @rdname padding
#' @export
`left_padding<-` <- function(ht, value) {
  prop_replace(ht, value, "left_padding")
}

#' @rdname padding
#' @export
set_left_padding <- function(ht, row, col, value) {
  prop_set(ht, row, col, value, "left_padding")
}

#' @rdname padding
#' @export
map_left_padding <- function(ht, row, col, fn) {
  prop_map(ht, row, col, fn, "left_padding")
}

#' @rdname padding
#' @export
right_padding <- function(ht) prop_get(ht, "right_padding")

#' @rdname padding
#' @export
`right_padding<-` <- function(ht, value) {
  prop_replace(ht, value, "right_padding")
}

#' @rdname padding
#' @export
set_right_padding <- function(ht, row, col, value) {
  prop_set(ht, row, col, value, "right_padding")
}

#' @rdname padding
#' @export
map_right_padding <- function(ht, row, col, fn) {
  prop_map(ht, row, col, fn, "right_padding")
}

#' @rdname padding
#' @export
top_padding <- function(ht) prop_get(ht, "top_padding")

#' @rdname padding
#' @export
`top_padding<-` <- function(ht, value) {
  prop_replace(ht, value, "top_padding")
}

#' @rdname padding
#' @export
set_top_padding <- function(ht, row, col, value) {
  prop_set(ht, row, col, value, "top_padding")
}

#' @rdname padding
#' @export
map_top_padding <- function(ht, row, col, fn) {
  prop_map(ht, row, col, fn, "top_padding")
}

#' @rdname padding
#' @export
bottom_padding <- function(ht) prop_get(ht, "bottom_padding")

#' @rdname padding
#' @export
`bottom_padding<-` <- function(ht, value) {
  prop_replace(ht, value, "bottom_padding")
}

#' @rdname padding
#' @export
set_bottom_padding <- function(ht, row, col, value) {
  prop_set(ht, row, col, value, "bottom_padding")
}

#' @rdname padding
#' @export
map_bottom_padding <- function(ht, row, col, fn) {
  prop_map(ht, row, col, fn, "bottom_padding")
}
