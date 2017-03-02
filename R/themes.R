

#' Theme a Huxtable
#'
#' These functions quickly set default styles for a huxtable.
#'
#' \code{theme_minimal} is a minimal theme with a simple line under headers.
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
#' @return The huxtable object.
#' @name themes
#'
#' @examples
NULL

#' @export
#' @rdname themes
theme_minimal <- function (ht, header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  if (header_row) bottom_border(ht)[1,] <- 1
  if (header_col) right_border(ht)[,1] <- 1

  ht
}

#' @export
#' @rdname themes
#' @param stripe Background colour for alternate rows
theme_striped <- function (ht, stripe = grey(.9), header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  background_color(ht)[seq(1, nrow(ht), 2),] <- 'white'
  if (nrow(ht) >= 2) background_color(ht)[seq(2, nrow(ht), 2),] <- stripe
  if (header_row) {
    background_color(ht)[1,] <- 'black'
    text_color(ht)[1,] <- 'white'
    bold(ht)[1,] <- TRUE
  }
  if (header_col) {
    background_color(ht)[,1] <- 'black'
    text_color(ht)[,1] <- 'white'
    bold(ht)[,1] <- TRUE
  }

  ht
}

theme_article <- function(ht, header_row = TRUE, header_col = TRUE) {
  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  top_border(ht)[1,] <- 1
  bottom_border(ht)[nrow(ht),] <- 1
  if (header_row) {
    bottom_border(ht)[1,] <- 1
    bold(ht)[1,] <- TRUE
  }
  if (header_col) bold(ht)[,1] <- TRUE
}

