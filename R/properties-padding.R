#' Set padding
#'
#' These functions set the space around the edges of cells, within the borders.
#'
#' @param ht A huxtable.
#' @seealso [set-multiple], [set-outer].
#'
#' @examples
#' left_padding(jams) <- 2
#' left_padding(jams)
#'
#' jams <- set_left_padding(jams, 2)
#' left_padding(jams)
#'
#' @name padding
#' @rdname padding
#' @export
left_padding <- function(ht) .prop_get(ht, "left_padding")

#' @rdname padding
#' @inheritParams left_padding
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("left_padding")`
#' @export
set_left_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "left_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @param fn A mapping function. See [mapping-functions] for details.
#' @export
map_left_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "left_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("left_padding")`
#' @export
`left_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "left_padding")
}

#' @rdname padding
right_padding <- function(ht) .prop_get(ht, "right_padding")

#' @rdname padding
#' @inheritParams left_padding
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("right_padding")`
#' @export
set_right_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "right_padding")
}

#' @rdname padding
#' @inheritParams set_right_padding
#' @export
map_right_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "right_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("right_padding")`
#' @export
`right_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "right_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @export
top_padding <- function(ht) .prop_get(ht, "top_padding")

#' @rdname padding
#' @inheritParams left_padding
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("top_padding")`
#' @export
set_top_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "top_padding")
}

#' @rdname padding
#' @inheritParams set_top_padding
#' @export
map_top_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "top_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("top_padding")`
#' @export
`top_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "top_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @export
bottom_padding <- function(ht) .prop_get(ht, "bottom_padding")

#' @rdname padding
#' @inheritParams left_padding
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("bottom_padding")`
#' @export
set_bottom_padding <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "bottom_padding")
}

#' @rdname padding
#' @inheritParams set_bottom_padding
#' @export
map_bottom_padding <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "bottom_padding")
}

#' @rdname padding
#' @inheritParams left_padding
#' @param value Numeric: padding width/height in points.
#'   `r .rd_default("bottom_padding")`
#' @export
`bottom_padding<-` <- function(ht, value) {
  .prop_replace(ht, value, "bottom_padding")
}
