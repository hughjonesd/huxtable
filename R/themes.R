

#' Theme a huxtable
#'
#' These functions quickly set default styles for a huxtable.
#'
#' `theme_plain` is a simple theme with a bold header, a grey striped background, and an outer border.
#'
#' `theme_basic` just adds a border for header rows and/or columns.
#'
#' `theme_striped` uses different backgrounds for alternate rows, and for headers.
#'
#' `theme_article` is similar to the style of many scientific journals.
#'   It sets horizontal lines above and below the table.
#'
#' `theme_mondrian` mimics the style of a Mondrian painting, with black borders and randomized
#'   colors.
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
#' @param position 'left', 'center' or 'right'
theme_plain <- function(ht, position = 'left'){
  ht <- set_outer_borders(ht, 0.4)
  ht <- set_background_color(ht, evens, everywhere, "#F2F2F2")
  ht <- set_bold(ht, 1, everywhere, TRUE)
  ht <- set_bottom_border(ht, 1, everywhere, 0.4)
  ht <- set_position(ht, position)

  ht
}

#' @export
#' @rdname themes
theme_basic <- function (ht, header_row = TRUE, header_col = TRUE) {
  assert_that(is.flag(header_row), is.flag(header_col))

  ht <- set_all_borders(ht, 1:nrow(ht), 1:ncol(ht), 0)
  if (header_row) bottom_border(ht)[1, ] <- 1
  if (header_col) right_border(ht)[, 1] <- 1

  ht
}


#' @export
#' @rdname themes
#' @param stripe Background colour for alternate rows
theme_striped <- function (ht, stripe = grDevices::grey(.9), header_row = TRUE, header_col = TRUE) {
  assert_that(is.flag(header_row), is.flag(header_col))

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
  assert_that(is.flag(header_row), is.flag(header_col))

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


#' @export
#' @rdname themes
#' @param prop_colored Roughly what proportion of cells should have a primary-color background?
#' @param font Font to use. For LaTeX, try `"cmss"`.
theme_mondrian <- function(ht, prop_colored = 0.1, font = 'Arial') {
  assert_that(is.number(prop_colored), prop_colored >= 0, prop_colored <= 1)

  ht <- set_all_borders(ht, 2)
  ht <- set_all_border_colors(ht, 'black')
  colored <- runif(nrow(ht) * ncol(ht)) <= prop_colored
  ncells <- nrow(ht) * ncol(ht)
  colored <- sample.int(ncells, size = ceiling(ncells * prop_colored), replace = FALSE)
  colors <- sample(c('red', 'blue', 'yellow'), length(colored), replace = TRUE)
  background_color(ht)[colored] <- colors
  font(ht) <- font

  ht
}
