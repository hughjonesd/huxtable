
theme_maker <- function (
        col1,
        col2,
        border_color = "white",
        header_color = col1,
        header_text = NA
      ) {

  theme_fn <- function (
          ht,
          header_rows = TRUE,
          header_cols = TRUE
        ) {
    ht <- set_all_borders(ht, 1)
    ht <- set_all_border_colors(ht, border_color)
    ht <- map_background_color(ht, by_rows(col1, col2))
    if (header_rows) {
      bold(ht)[header_rows(ht), ]             <- TRUE
      background_color(ht)[header_rows(ht), ] <- header_color
      text_color(ht)[header_rows(ht), ]       <- header_text
    }
    if (header_cols) {
      bold(ht)[, header_cols(ht)]             <- TRUE
      background_color(ht)[, header_cols(ht)] <- header_color
      text_color(ht)[, header_cols(ht)]       <- header_text
    }
    ht <- clean_outer_padding(ht, 6)

    ht
  }

  return(theme_fn)
}


#' Theme a huxtable
#'
#' These functions quickly set default styles for a huxtable.
#'
#' * `theme_plain` is a simple theme with a bold header, a grey striped
#'   background, and an outer border.
#'
#' * `theme_basic` sets header rows/columns to bold, and adds a border beneath
#'   them.
#'
#' * `theme_compact` is like `theme_basic` but with minimal padding.
#'
#' * `theme_striped` uses different backgrounds for alternate rows, and for
#'   headers.
#'
#' * `theme_article` is similar to the style of many scientific journals.
#'    It sets horizontal lines above and below the table.
#'
#'  * `theme_bright` uses thick white borders and a colourful header. It
#'     works nicely with sans-serif fonts.
#'
#'  * `theme_grey`, `theme_blue`, `theme_orange` and `theme_green` use white
#'    borders and subtle horizontal stripes.
#'
#' * `theme_mondrian` mimics the style of a Mondrian painting, with thick black
#'    borders and randomized colors.
#'
#' @param ht A huxtable object.
#' @param header_rows Logical: style header rows?
#' @param header_cols Logical: style header columns?
#'
#' @return The huxtable object, appropriately styled.
#' @name themes
#'
#' @examples
#'
#' theme_plain(jams)
#' theme_basic(jams)
#' theme_compact(jams)
#' theme_striped(jams)
#' theme_article(jams)
#' theme_bright(jams)
#' theme_grey(jams)
#' theme_blue(jams)
#' theme_orange(jams)
#' theme_green(jams)
#' theme_mondrian(jams)
#' \dontrun{
#'   quick_pdf(
#'           theme_plain(jams),
#'           theme_basic(jams),
#'           theme_compact(jams)
#'           theme_striped(jams),
#'           theme_article(jams),
#'           theme_bright(jams),
#'           theme_grey(jams),
#'           theme_blue(jams),
#'           theme_orange(jams),
#'           theme_green(jams),
#'           theme_mondrian(jams)
#'         )
#' }
#'
NULL


#' @export
#' @rdname themes
#' @param position "left", "center" or "right"
theme_plain <- function(
        ht,
        header_rows = TRUE,
        position = "center"
      ){
  ht <- set_outer_borders(ht)
  ht <- set_background_color(ht, evens, everywhere, "#F2F2F2")
  if (header_rows) {
    ht <- set_bold(ht, header_rows(ht), everywhere, TRUE)
    ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere, 0.4)
  }
  ht <- set_position(ht, position)

  ht
}


#' @export
#' @rdname themes
#' @param colors Colors for header rows. Can also be a palette function.
theme_bright <- function (
        ht,
        header_rows = TRUE,
        header_cols = FALSE,
        colors = c("#7eabf2", "#e376e3", "#fcbb03", "#7aba59", "#fc0356"))
      {
  assert_that(is_hux(ht), is.flag(header_rows), is.flag(header_cols),
      is.character(colors) || is.function(colors))

  if (is.function(colors)) colors <- colors(ncol(ht))
  ht <- set_all_borders(ht, 3)
  ht <-  set_all_border_colors(ht, "white")
  if (header_rows) {
    ht <-  map_background_color(ht, header_rows(ht),
          everywhere, by_cols(colors))
    ht <-  set_text_color(ht, header_rows(ht), everywhere, "white")
  }
  if (header_cols) {
    ht <-  map_background_color(ht, everywhere, header_cols(ht),
          by_rows(colors))
    ht <-  set_text_color(ht, everywhere, header_cols(ht), "white")
  }

  ht
}


#' @export
#' @rdname themes
theme_basic <- function (
        ht,
        header_rows = TRUE,
        header_cols = FALSE
      ) {
  assert_that(is.flag(header_rows), is.flag(header_cols))

  ht <- set_all_borders(ht, 0)
  if (header_rows) {
    ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere)
    ht <- set_bold(ht, header_rows(ht), everywhere)
  }
  if (header_cols) {
    ht <- set_right_border(ht, everywhere, largest(header_cols(ht)))
    ht <- set_bold(ht, everywhere, header_cols(ht))
  }

  ht <- clean_outer_padding(ht, 2)

  ht
}


#' @export
#' @rdname themes
theme_compact <- function (
        ht,
        header_rows = TRUE,
        header_cols = FALSE
      ) {
  assert_that(is.flag(header_rows), is.flag(header_cols))

  ht <- set_all_borders(ht, 0)
  if (header_rows) {
    ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere)
    ht <- style_header_rows(ht, bold = TRUE)
  }
  if (header_cols) {
    ht <- style_header_cols(ht, bold = TRUE)
  }
  ht <- set_all_padding(ht, 1)
  ht <- clean_outer_padding(ht, 0)

  ht
}


#' @export
#' @rdname themes
#' @param stripe  Background colour for odd rows
#' @param stripe2 Background colour for even rows
theme_striped <- function (
        ht,
        stripe = "grey90",
        stripe2 = "grey95",
        header_rows = TRUE,
        header_cols = TRUE
      ) {
  assert_that(is.flag(header_rows), is.flag(header_cols))

  ht <- set_all_borders(ht)
  ht <- set_all_border_colors(ht, "white")
  ht <- map_background_color(ht, by_rows(stripe, stripe2))

  if (header_rows) {
    ht <- style_header_rows(ht, bold = TRUE)
  }
  if (header_cols) {
    ht <- style_header_cols(ht,
            bold = TRUE,
            background_color = stripe
          )
  }

  ht
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
theme_article <- function (
        ht,
        header_rows = TRUE,
        header_cols = TRUE
      ) {
  assert_that(is.flag(header_rows), is.flag(header_cols))

  ht <- set_all_borders(ht, 0)
  ht <- set_top_border(ht, 1, everywhere)
  ht <- set_bottom_border(ht, final(1), everywhere)
  if (header_rows) {
    ht <- set_bottom_border(ht, largest(header_rows(ht)), everywhere)
    ht <- style_header_rows(ht, bold = TRUE)
  }
  if (header_cols) {
    ht <- style_header_cols(ht, bold = TRUE)
  }

  ht <- clean_outer_padding(ht, 0)

  ht
}


#' @export
#' @rdname themes
#' @param prop_colored Roughly what proportion of cells should have
#'   a primary-color background?
#' @param font Font to use. For LaTeX, try `"cmss"`.
theme_mondrian <- function (
        ht,
        prop_colored = 0.1,
        font = NULL
      ) {
  assert_that(is.number(prop_colored), prop_colored >= 0, prop_colored <= 1)

  ht <- set_all_borders(ht, 2)
  ht <- set_all_padding(ht, 3)
  ht <- set_all_border_colors(ht, "black")
  ncells <- nrow(ht) * ncol(ht)
  colored <- sample.int(ncells, size = ceiling(ncells * prop_colored), replace = FALSE)
  colors <- sample(c("red", "blue", "yellow"), length(colored), replace = TRUE)
  background_color(ht)[colored] <- colors
  if (! is.null(font)) font(ht) <- font

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
  xs <- c(x[-1], FALSE)
  which(x & ! xs)
}
