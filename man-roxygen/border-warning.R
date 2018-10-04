
#' @section Note:
#' huxtable currently sets borders on specific cells. This can lead to surprising behaviour when
#' cells span multiple rows or columns: see the example. This behaviour may be improved in a future
#' release.
#'
#' @examples
#' # When cells span multiple rows:
#' ht <- tribble_hux(
#'   ~Col1,                   ~Col2,
#'   "Cell 1,1 spans 2 rows", "Cell 1,2",
#'   "Cell 2,1 is invisible", "Cell 2,2"
#' )
#'
#' rowspan(ht)[1, 1] <- 2
#' ht
#'
#' bottom_border(ht)[2, ] <- 1
#' bottom_border_color(ht)[2, ] <- 'red'
#'
#' # Cell 1, 1 does not have a border set:
#' ht
#'
#' # Fixed:
#' bottom_border(ht)[1, 1] <- 1
#' bottom_border_color(ht)[1, 1] <- 'red'
#' ht
