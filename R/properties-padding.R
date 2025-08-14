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
  prop_set(ht, "left_padding", value = value)
}

#' @rdname padding
#' @export
set_left_padding <- function(ht, row, col, value) {
  prop_set(ht, "left_padding", row, col, value = value)
}

#' @rdname padding
#' @export
map_left_padding <- function(ht, row, col, fn) {
  prop_set(ht, "left_padding", row, col, fn = fn)
}

#' @rdname padding
#' @export
right_padding <- function(ht) prop_get(ht, "right_padding")

#' @rdname padding
#' @export
`right_padding<-` <- function(ht, value) {
  prop_set(ht, "right_padding", value = value)
}

#' @rdname padding
#' @export
set_right_padding <- function(ht, row, col, value) {
  prop_set(ht, "right_padding", row, col, value = value)
}

#' @rdname padding
#' @export
map_right_padding <- function(ht, row, col, fn) {
  prop_set(ht, "right_padding", row, col, fn = fn)
}

#' @rdname padding
#' @export
top_padding <- function(ht) prop_get(ht, "top_padding")

#' @rdname padding
#' @export
`top_padding<-` <- function(ht, value) {
  prop_set(ht, "top_padding", value = value)
}

#' @rdname padding
#' @export
set_top_padding <- function(ht, row, col, value) {
  prop_set(ht, "top_padding", row, col, value = value)
}

#' @rdname padding
#' @export
map_top_padding <- function(ht, row, col, fn) {
  prop_set(ht, "top_padding", row, col, fn = fn)
}

#' @rdname padding
#' @export
bottom_padding <- function(ht) prop_get(ht, "bottom_padding")

#' @rdname padding
#' @export
`bottom_padding<-` <- function(ht, value) {
  prop_set(ht, "bottom_padding", value = value)
}

#' @rdname padding
#' @export
set_bottom_padding <- function(ht, row, col, value) {
  prop_set(ht, "bottom_padding", row, col, value = value)
}

#' @rdname padding
#' @export
map_bottom_padding <- function(ht, row, col, fn) {
  prop_set(ht, "bottom_padding", row, col, fn = fn)
}
