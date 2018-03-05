

#' Theme a huxtable
#'
#' These functions quickly set default styles for a huxtable.
#'
#' \code{theme_plain} is a simple theme with a bold header, a grey striped background, and an outer border.
#'
#' \code{theme_basic} just adds a border for header rows and/or columns.
#'
#' \code{theme_striped} uses different backgrounds for alternate rows, and for headers.
#'
#' \code{theme_article} is similar to the style of many scientific journals.
#' It sets horizontal lines above and below the table.
#'
#' @param ht A huxtable object.
#' @param header_row Logical: style first row differently?
#' @param header_col Logical: style first column differently?
#'
#' @return The huxtable object, appropriately styled.
#' @name themes
#'
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5)
#' theme_striped(ht)
NULL


#' @export
#' @rdname themes
theme_plain <- function(ht){
  ht <- set_outer_borders(ht, 0.4)
  ht <- set_background_color(ht, evens, everywhere, "#F2F2F2")
  ht <- set_bold(ht, 1, everywhere, TRUE)
  ht <- set_bottom_border(ht, 1, everywhere, 0.4)
  ht <- set_left_padding(ht, 10)
  ht <- set_right_padding(ht, 10)
  ht <- set_top_padding(ht, 6)
  ht <- set_bottom_padding(ht, 6)

  ht
}

#' @export
#' @rdname themes
theme_basic <- function (ht, header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  if (header_row) bottom_border(ht)[1, ] <- 1
  if (header_col) right_border(ht)[, 1] <- 1

  ht
}


#' @export
#' @rdname themes
#' @param stripe Background colour for alternate rows
theme_striped <- function (ht, stripe = grDevices::grey(.9), header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  background_color(ht)[seq(1, nrow(ht), 2), ] <- 'white'
  if (nrow(ht) >= 2) background_color(ht)[seq(2, nrow(ht), 2), ] <- stripe
  if (header_row) {
    background_color(ht)[1, ] <- 'black'
    text_color(ht)[1, ]       <- 'white'
    ht <- set_all_border_colors(ht, 1, every(), 'white')
    bold(ht)[1, ]             <- TRUE
  }
  if (header_col) {
    background_color(ht)[, 1] <- 'black'
    text_color(ht)[, 1]       <- 'white'
    ht <- set_all_border_colors(ht, every(), 1, 'white')
    bold(ht)[, 1]             <- TRUE
  }

  ht
}


#' @export
#' @rdname themes
theme_article <- function(ht, header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  top_border(ht)[1, ] <- 1
  bottom_border(ht)[nrow(ht), ] <- 1
  if (header_row) {
    bottom_border(ht)[1, ] <- 1
    bold(ht)[1, ] <- TRUE
  }
  if (header_col) bold(ht)[, 1] <- TRUE

  ht
}
