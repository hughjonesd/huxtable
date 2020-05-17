

#' Theme a huxtable
#'
#' These functions quickly set default styles for a huxtable.
#'
#' `theme_plain` is a simple theme with a bold header, a grey striped background, and an outer border.
#'
#' `theme_basic` just sets header rows/columns to bold and adds a border beneath them
#'    as well as a bottom border.
#'
#' `theme_striped` uses different backgrounds for alternate rows, and for headers.
#'
#' `theme_article` is similar to the style of many scientific journals.
#'   It sets horizontal lines above and below the table.
#'
#'  `theme_bright` uses thick white borders and a colourful header. It
#'  works nicely with sans-serif fonts.
#'
#'  `theme_grey`, `theme_blue`, `theme_orange` and `theme_green` use white borders and subtle
#'    horizontal stripes.
#'
#' `theme_mondrian` mimics the style of a Mondrian painting, with black borders and randomized
#'   colors.
#'
#' @param ht A huxtable object.
#' @param header_row Logical: set first row to header, and style header rows?
#' @param header_col Logical: set first column to header, and style header
#'   columns?
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
  ht <- set_bold(ht, header_rows(ht), everywhere, TRUE)
  ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere, 0.4)
  ht <- set_position(ht, position)

  ht
}


#' @export
#' @rdname themes
#' @param colors Colors for header rows. Can also be a palette function.
theme_bright <- function (ht,
        header_row = TRUE,
        header_col = FALSE,
        colors = c("#7eabf2", "#e376e3", "#fcbb03", "#7aba59", "#fc0356"))
      {
  if (is.function(colors)) colors <- colors(ncol(ht))
  ht <- set_all_borders(ht, 3)
  ht <-  set_all_border_colors(ht, "white")
  if (header_row) {
    ht <- set_header_rows(ht, 1, TRUE)
    ht <-  map_background_color(ht, 1, everywhere, by_cols(colors))
    ht <-  set_text_color(ht, 1, everywhere, "white")
  }
  if (header_col) {
    ht <- set_header_cols(ht, 1, TRUE)
    ht <-  map_background_color(ht, everywhere, 1, by_rows(colors))
    ht <-  set_text_color(ht, everywhere, 1, "white")
  }

  ht
}


#' @export
#' @rdname themes
theme_basic <- function (ht, header_row = TRUE, header_col = FALSE) {
  assert_that(is.flag(header_row), is.flag(header_col))

  ht <- set_all_borders(ht, 0)
  if (header_row) {
    ht <- set_header_rows(ht, 1, TRUE)
    ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere)
    ht <- set_bold(ht, header_rows(ht), everywhere)
  }
  if (header_col) {
    ht <- set_header_cols(ht, 1, TRUE)
    ht <- set_right_border(ht, everywhere, largest(header_cols(ht)))
    ht <- set_bold(ht, everywhere, header_cols(ht))
  }

  ht <- set_bottom_border(ht, final(1), everywhere)
  ht <- clean_outer_padding(ht, 2)

  ht
}


#' @export
#' @rdname themes
#' @param stripe  Background colour for odd rows
#' @param stripe2 Background colour for even rows
theme_striped <- function (ht, stripe = "grey90",
      stripe2 = "grey95", header_row = TRUE, header_col = TRUE) {
  assert_that(is.flag(header_row), is.flag(header_col))

  ht <- set_all_borders(ht)
  ht <- set_all_border_colors(ht, "white")
  ht <- map_background_color(ht, by_rows(stripe, stripe2))

  if (header_row) {
    ht <- set_header_rows(ht, 1, TRUE)
    ht <- style_header_rows(ht,
          bold = TRUE)
  }
  if (header_col) {
    ht <- set_header_cols(ht, 1, TRUE)
    ht <- style_header_cols(ht,
            bold = TRUE,
            background_color = stripe
          )
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
      ht <- set_header_rows(ht, 1, TRUE)
      bold(ht)[header_rows(ht), ]             <- TRUE
      background_color(ht)[header_rows(ht), ] <- header_color
      text_color(ht)[header_rows(ht), ]       <- header_text
    }
    if (header_col) {
      ht <- set_header_cols(ht, 1, TRUE)
      bold(ht)[, header_cols(ht)]             <- TRUE
      background_color(ht)[, header_cols(ht)] <- header_color
      text_color(ht)[, header_cols(ht)]       <- header_text
    }
    ht <- clean_outer_padding(ht, 4)

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

  ht <- set_all_borders(ht, 0)
  top_border(ht)[1, ] <- 0.4
  bottom_border(ht)[nrow(ht), ] <- 0.4
  if (header_row) {
    ht <- set_header_rows(ht, 1, TRUE)
    bottom_border(ht)[header_rows(ht), ] <- 0.4
    bold(ht)[header_rows(ht), ] <- TRUE
  }
  if (header_col) {
    ht <- set_header_cols(ht, 1, TRUE)
    bold(ht)[, header_cols(ht)] <- TRUE
  }
  ht <- clean_outer_padding(ht, 0)

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


clean_outer_padding <- function (ht, pad) {
  # a tight pad works best for blank themes with dark borders;
  # for coloured backgrounds you don't want that
  ht <- set_left_padding(ht, everywhere, 1, pad)
  ht <- set_right_padding(ht, everywhere, final(1), pad)

  ht
}


largest <- function(x) {
  if (any(x)) max(which(x)) else integer(0)
}
