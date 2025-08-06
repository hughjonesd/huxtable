#' Set background color stripes
#'
#' These convenience functions call [map_background_color] with [by_rows]
#' or [by_cols].
#'
#' @inherit left_border params return
#' @param stripe1 Color for rows/columns 1, 3, 5, ...
#' @param stripe2 Color for rows/columns 2, 4, 6, ...
#'
#' @examples
#'
#' stripe_rows(jams)
#' stripe_columns(jams)
#' stripe_rows(jams, "red", "blue")
#'
#' @name stripes
NULL


#' @rdname stripes
#' @export
stripe_rows <- function(ht, stripe1 = "white", stripe2 = "grey90") {
  map_background_color(ht = ht, by_rows(stripe1, stripe2))
}


#' @rdname stripes
#' @export
stripe_columns <- function(ht, stripe1 = "white", stripe2 = "grey90") {
  map_background_color(ht = ht, by_cols(stripe1, stripe2))
}
