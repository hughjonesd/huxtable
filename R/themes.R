

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
#'  `theme_grey`, `theme_blue`, `theme_orange` and `theme_green` use white borders and subtle
#'    horizontal stripes.
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
#'
#' theme_striped(jams)
#' theme_plain(jams)
#' theme_basic(jams)
#' theme_article(jams)
#' theme_mondrian(jams)
#' theme_grey(jams)
#' theme_blue(jams)
#' theme_orange(jams)
#' theme_green(jams)
#' \dontrun{
#'   quick_pdf(
#'           theme_striped(jams),
#'           theme_plain(jams),
#'           theme_basic(jams),
#'           theme_article(jams),
#'           theme_mondrian(jams),
#'           theme_grey(jams),
#'           theme_blue(jams),
#'           theme_orange(jams),
#'           theme_green(jams)
#'         )
#' }
NULL


#' @export
#' @rdname themes
#' @param position "left", "center" or "right"
theme_plain <- function(ht, position = "center"){
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
  background_color(ht)[seq(1, nrow(ht), 2), ] <- "white"
  if (nrow(ht) >= 2) background_color(ht)[seq(2, nrow(ht), 2), ] <- stripe
  if (header_row) {
    background_color(ht)[1, ] <- "black"
    text_color(ht)[1, ]       <- "white"
    ht <- set_all_border_colors(ht, 1, everywhere, "white")
    bold(ht)[1, ]             <- TRUE
  }
  if (header_col) {
    background_color(ht)[, 1] <- "black"
    text_color(ht)[, 1]       <- "white"
    ht <- set_all_border_colors(ht, everywhere, 1, "white")
    bold(ht)[, 1]             <- TRUE
  }

  ht
}


theme_maker <- function (
        col1,
        col2,
        border_color = "white",
        header_color = col1,
        header_text = NA
      ) {
  function (ht, header_row = TRUE, header_col = TRUE) {
    ht <- set_all_borders(ht, 1)
    ht <- set_all_border_colors(ht, border_color)
    ht <- map_background_color(ht, by_rows(col1, col2))
    if (header_row) {
      bold(ht)[1, ]             <- TRUE
      background_color(ht)[1, ] <- header_color
      text_color(ht)[1, ]       <- header_text
    }
    if (header_col) {
      bold(ht)[, 1]             <- TRUE
      background_color(ht)[, 1] <- header_color
      text_color(ht)[, 1]       <- header_text
    }

    ht
  }
}


#' @export
#' @rdname themes
theme_grey <- theme_maker(
        col1         = grDevices::grey(.9),
        col2         = grDevices::grey(.95),
        header_color = grDevices::grey(.8)
      )


#' @export
#' @rdname themes
theme_blue <- theme_maker(
        col1         = "#A9CCE3",
        col2         = "#D4E6F1",
        header_color = "#5499C7",
        header_text  = "white"
      )


#' @export
#' @rdname themes
theme_orange <- theme_maker(
        col1         = "#F5CBA7",
        col2         = "#FAE5D3",
        header_color = "#D0D3D4"
      )


#' @export
#' @rdname themes
theme_green <- theme_maker(
        col1         = "#C8E6C9",
        col2         = "#A5D6A7",
        header_color = "#4CAF50",
        header_text  = "white"
      )


#' @export
#' @rdname themes
theme_article <- function (ht, header_row = TRUE, header_col = TRUE) {
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
theme_mondrian <- function (ht, prop_colored = 0.1, font = "Arial") {
  assert_that(is.number(prop_colored), prop_colored >= 0, prop_colored <= 1)

  ht <- set_all_borders(ht, 2)
  ht <- set_all_border_colors(ht, "black")
  ncells <- nrow(ht) * ncol(ht)
  colored <- sample.int(ncells, size = ceiling(ncells * prop_colored), replace = FALSE)
  colors <- sample(c("red", "blue", "yellow"), length(colored), replace = TRUE)
  background_color(ht)[colored] <- colors
  font(ht) <- font

  ht
}
