#' Set the vertical alignment of cell content
#'
#' Allowed values are "top", "middle", "bottom" or `NA`.
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("valign")`
#'
#' @details
#' Vertical alignment may not work for short text in LaTeX.
#' Defining row heights with [row_height()] may help.
#'
#' @examples
#' valign(jams) <- "top"
#' valign(jams)
#'
#' jams2 <- set_valign(jams, "bottom")
#' valign(jams2)
#'
#' jams3 <- set_valign(jams, 2:3, 1, "bottom")
#' valign(jams3)
#'
#' jams4 <- map_valign(jams, by_rows(
#'   "bottom",
#'   "top"
#' ))
#' valign(jams4)
#'
#' @name valign
NULL

#' @rdname valign
#' @export
valign <- function(ht) .prop_get(ht, "valign")

#' @rdname valign
#' @export
`valign<-` <- function(ht, value) {
  .prop_replace(ht, value, "valign",
    check_fun = is.character,
    check_values = c("top", "middle", "bottom")
  )
}

#' @rdname valign
#' @export
set_valign <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "valign",
    check_fun = is.character,
    check_values = c("top", "middle", "bottom")
  )
}

#' @rdname valign
#' @export
map_valign <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "valign",
    check_fun = is.character,
    check_values = c("top", "middle", "bottom")
  )
}


#' @noRd
check_align_value <- function(x) {
  x <- na.omit(x)
  is.character(x) && all(x %in% c("left", "centre", "center", "right") | ncharw(x) == 1)
}


#' Set the horizontal alignment of cell content
#'
#' Values may be "left", "center", "right", `NA` or a single character. If
#' `value` is a single character (e.g. a decimal point), then the cell is
#' aligned on this character.
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("align")`
#'
#' @section Aligning on a decimal point:
#'
#' To align cells on the decimal point, set `align` to `"."` or any other single
#' character (e.g. `","` in European languages).
#'
#' By default, huxtable aligns these cells by padding with spaces. The mechanics
#' of this were improved for LaTeX in version 5.3.0, but are still not perfect.
#' Using a fixed-width font may help.
#'
#' If `options("huxtable.latex_siunitx_align")` is set to `TRUE`, then in
#' LaTeX output, numbers in these cells will be surrounded by `\\tablenum{}`.
#' See the siunitx documentation for more details. Note that this may have
#' other side-effects, for example `1e3` becomes `1 x 10^3`.
#'
#' To use non-default decimal points, set both `align(ht)` and
#' [number_format()]. See the example.
#'
#' @examples
#'
#' numbers <- c(1, 1.5, 1.03, 10, 10.01)
#' number_hux <- as_hux(matrix(numbers, 5, 5))
#' number_format(number_hux) <- "%.4g"
#' number_format(number_hux)[, 5] <- fmt_pretty(
#'   decimal.mark = ",",
#'   big.mark = ""
#' )
#'
#' number_hux <- map_align(
#'   number_hux,
#'   by_cols("left", "center", "right", ".", ",")
#' )
#'
#' alignments <- c(
#'   "left",
#'   "centre",
#'   "right",
#'   "decimal (.)",
#'   "decimal (,)"
#' )
#' number_hux <- rbind(
#'   alignments,
#'   number_hux
#' )
#'
#' align(number_hux)
#' number_hux
#'
#' @name align
NULL

#' @rdname align
#' @export
align <- function(ht) .prop_get(ht, "align")

#' @rdname align
#' @export
`align<-` <- function(ht, value) {
  .prop_replace(ht, value, "align",
    check_fun = check_align_value,
    extra = quote(value[value == "centre"] <- "center")
  )
}

#' @rdname align
#' @export
set_align <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "align",
    check_fun = check_align_value,
    extra = quote(value[value == "centre"] <- "center")
  )
}

#' @rdname align
#' @export
map_align <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "align",
    check_fun = check_align_value,
    extra = quote(value[value == "centre"] <- "center")
  )
}


#' Extend cells over multiple rows and/or columns
#'
#' A cell with rowspan of 2 covers the cell directly below it. A cell with
#' colspan of 2 covers the cell directly to its right. A cell with rowspan of 2
#' and colspan of 2 covers a 2 x 2 square, hiding three other cells.
#'
#' @inheritParams .hux_prop_params
#' @param value An integer vector or matrix.
#'
#' @inheritSection span-overwrites-shadows Cell content
#'
#' @seealso [merge_cells()], [merge_across()] and [merge_down()] for
#' a higher-level interface.
#'
#' @examples
#'
#' letter_hux <- as_hux(matrix(LETTERS[1:9], 3, 3))
#' letter_hux <- set_all_borders(letter_hux)
#' letter_hux
#' set_rowspan(letter_hux, 1, 1, 2)
#' set_colspan(letter_hux, 1, 1, 2)
#'
#' @name spans
NULL


#' @rdname spans
#' @export
rowspan <- function(ht) .prop_get(ht, "rowspan")

#' @rdname spans
#' @export
`rowspan<-` <- function(ht, value) {
  .prop_replace(ht, value, "rowspan",
    check_fun = is.numeric,
    extra = quote({
      too_long <- na.omit(base::row(ht) + value - 1 > nrow(ht))
      if (any(too_long)) {
        stop("rowspan would extend beyond bottom of table")
      }
      dc <- display_cells(ht, new_rowspan = value)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}

#' @rdname spans
#' @export
set_rowspan <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "rowspan",
    check_fun = is.numeric,
    extra = quote({
      rows <- base::row(ht)[rc$row, rc$col, drop = FALSE]
      too_long <- na.omit(rows + value - 1 > nrow(ht))
      if (any(too_long)) {
        stop("rowspan would extend beyond bottom of table")
      }
      new_rs <- attr(ht, "rowspan")
      new_rs[rc$row, rc$col] <- value
      dc <- display_cells(ht, new_rowspan = new_rs)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}

#' @rdname spans
#' @export
map_rowspan <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "rowspan",
    check_fun = is.numeric,
    extra = quote({
      rows <- base::row(ht)[rc$row, rc$col, drop = FALSE]
      too_long <- na.omit(rows + value - 1 > nrow(ht))
      if (any(too_long)) {
        stop("rowspan would extend beyond bottom of table")
      }
      new_rs <- attr(ht, "rowspan")
      new_rs[rc$row, rc$col] <- value
      dc <- display_cells(ht, new_rowspan = new_rs)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}


#' @rdname spans
#' @export
colspan <- function(ht) .prop_get(ht, "colspan")

#' @rdname spans
#' @export
`colspan<-` <- function(ht, value) {
  .prop_replace(ht, value, "colspan",
    check_fun = is.numeric,
    extra = quote({
      too_long <- na.omit(base::col(ht) + value - 1 > ncol(ht))
      if (any(too_long)) {
        stop("colspan would extend beyond right edge of table")
      }
      dc <- display_cells(ht, new_colspan = value)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}

#' @rdname spans
#' @export
set_colspan <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "colspan",
    check_fun = is.numeric,
    extra = quote({
      cols <- base::col(ht)[rc$row, rc$col, drop = FALSE]
      too_long <- na.omit(cols + value - 1 > ncol(ht))
      if (any(too_long)) {
        stop("colspan would extend beyond right edge of table")
      }
      new_cs <- attr(ht, "colspan")
      new_cs[rc$row, rc$col] <- value
      dc <- display_cells(ht, new_colspan = new_cs)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}

#' @rdname spans
#' @export
map_colspan <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "colspan",
    check_fun = is.numeric,
    extra = quote({
      cols <- base::col(ht)[rc$row, rc$col, drop = FALSE]
      too_long <- na.omit(cols + value - 1 > ncol(ht))
      if (any(too_long)) {
        stop("colspan would extend beyond right edge of table")
      }
      new_cs <- attr(ht, "colspan")
      new_cs[rc$row, rc$col] <- value
      dc <- display_cells(ht, new_colspan = new_cs)
      if (any(value > 1)) {
        ht <- overwrite_shadowed_cells(ht, dc)
      }
    })
  )
}


#' @noRd
overwrite_shadowed_cells <- function(ht, dc) {
  dcells <- as.matrix(dc[, c("display_row", "display_col")])
  contents <- as.data.frame(ht)[dcells]
  ht[] <- contents

  ht
}


#' @description
#' Colors can be in any format understood by R:
#'
#' * A color name like `"darkred"`
#' * A HTML string like `"#FF0000"`
#' * The result of a function like `rgb(1, 0, 0)` or `grey(0.5)`
#' @name description-colors
NULL


#' Set cell background color
#'
#' @inherit description-colors description
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("background_color")`
#'
#' @details
#' Transparent colors are not guaranteed to work at present.
#'
#' @family formatting functions
#'
#' @examples
#' background_color(jams) <- grey(0.7)
#' background_color(jams)
#'
#' set_background_color(jams, "yellow")
#' set_background_color(jams, 2:3, 1, "yellow")
#' map_background_color(jams, by_rows("yellow", grey(0.7)))
#'
#' @name background_color
NULL

#' @rdname background_color
#' @export
background_color <- function(ht) .prop_get(ht, "background_color")

#' @rdname background_color
#' @export
`background_color<-` <- function(ht, value) {
  .prop_replace(ht, value, "background_color")
}

#' @rdname background_color
#' @export
set_background_color <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "background_color")
}

#' @rdname background_color
#' @export
map_background_color <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "background_color")
}


#' Set the color of text in cells
#'
#' @inherit description-colors description
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("text_color")`
#'
#' @family formatting functions
#'
#' @examples
#' text_color(jams) <- "blue"
#' text_color(jams)
#'
#' set_text_color(jams, "red")
#' set_text_color(jams, 2:3, 1, "red")
#' map_text_color(jams, by_rows("red", "blue"))
#'
#' @name text_color
NULL

#' @rdname text_color
#' @export
text_color <- function(ht) .prop_get(ht, "text_color")

#' @rdname text_color
#' @export
`text_color<-` <- function(ht, value) {
  .prop_replace(ht, value, "text_color")
}

#' @rdname text_color
#' @export
set_text_color <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "text_color")
}

#' @rdname text_color
#' @export
map_text_color <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "text_color")
}


#' Wrap cell content over multiple lines
#'
#' Text wrapping only works when the table [width()] has been set. In
#' particular, if you want to insert newlines in cells, then you should
#' set a value for [width()] and set `wrap` to `TRUE`.
#'
#' @inheritParams .hux_prop_params
#' @param value A logical vector or matrix. `r .rd_default("wrap")`
#'
#' @examples
#' long_text <- paste(
#'   rep("Some long text.", 10),
#'   collapse = " "
#' )
#' ht <- huxtable(Long = long_text)
#' width(ht) <- 0.2
#' wrap(ht) <- TRUE
#'
#' \dontrun{
#' quick_html(ht)
#' }
#'
#' @name wrap
NULL

#' @rdname wrap
#' @export
wrap <- function(ht) .prop_get(ht, "wrap")

#' @rdname wrap
#' @export
`wrap<-` <- function(ht, value) {
  .prop_replace(ht, value, "wrap", check_fun = is.logical)
}

#' @rdname wrap
#' @export
set_wrap <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "wrap", check_fun = is.logical)
}

#' @rdname wrap
#' @export
map_wrap <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "wrap", check_fun = is.logical)
}


#' Escape or unescape text in cells
#'
#' Setting `escape_contents` to `FALSE` allows you to include raw HTML or
#' TeX code in your cells.
#'
#' @inheritParams .hux_prop_params
#' @param value A logical vector or matrix. `r .rd_default("escape_contents")`
#'
#' @details
#' If [markdown()] is `TRUE` for a cell, the `escape_contents` property
#' will be ignored.
#'
#' @seealso [sanitize()] for escaping text manually.
#'
#' @examples
#' ht <- huxtable(
#'   Text   = "x squared",
#'   Maths  = "$x^2$"
#' )
#' ht <- set_escape_contents(ht, FALSE)
#' \dontrun{
#' quick_pdf(ht)
#' }
#'
#' @name escape_contents
NULL

#' @rdname escape_contents
#' @export
escape_contents <- function(ht) .prop_get(ht, "escape_contents")

#' @rdname escape_contents
#' @export
`escape_contents<-` <- function(ht, value) {
  .prop_replace(ht, value, "escape_contents", check_fun = is.logical)
}

#' @rdname escape_contents
#' @export
set_escape_contents <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "escape_contents", check_fun = is.logical)
}

#' @rdname escape_contents
#' @export
map_escape_contents <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "escape_contents", check_fun = is.logical)
}


#' Interpret cell content as markdown
#'
#' Cells where the markdown property is `TRUE` will be interpreted as
#' [markdown](https://commonmark.org/help/).
#'
#' @inheritParams .hux_prop_params
#' @param value A logical vector or matrix. `r .rd_default("markdown")`
#'
#' @details
#' Markdown is currently implemented for HTML, Word, Powerpoint, RTF, LaTeX and
#' on-screen display. Word requires the `ftExtra` package.
#'
#' Most formats use [commonmark](https://commonmark.org), with the
#' "strikethrough" extension enabled.
#'
#' The following features are intended to work:
#'
#' * bold and italic text
#' * strikethrough (write `~~text~~` to strike through text).
#' * hyperlinks
#'
#' There are some quirks:
#'
#' * Paragraph-level properties (e.g. lists) won't work in Word.
#' * Strikethrough will probably not work in Word.
#' * To make lists work in LaTeX, set [width()] and ensure [wrap()] is `TRUE`.
#' * Inline images in RTF work using the INCLUDEPICTURE field type.
#'
#' If you try to use markdown tables within a table cell, then seek psychiatric
#' help.
#'
#' @inheritSection markdown-note Note
#'
#' @seealso [set_markdown_contents()], a shortcut function.
#'
#' @examples
#' jams[3, 2] <- "~2.10~ **Sale!** 1.50"
#' set_markdown(jams, 3, 2)
#'
#' @name markdown
NULL

#' @rdname markdown
#' @export
markdown <- function(ht) .prop_get(ht, "markdown")

#' @rdname markdown
#' @export
`markdown<-` <- function(ht, value) {
  .prop_replace(ht, value, "markdown", check_fun = is.logical)
}

#' @rdname markdown
#' @export
set_markdown <- function(ht, row, col, value = TRUE) {
  .prop_set(ht, row, col, value, "markdown", check_fun = is.logical)
}

#' @rdname markdown
#' @export
map_markdown <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "markdown", check_fun = is.logical)
}



#' Change how NA values are printed
#'
#' NA values in the huxtable are printed as the value of `na_string`.
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("na_string")`
#'
#' @family formatting functions
#'
#' @examples
#' jams[3, 2] <- NA
#' jams
#' set_na_string(jams, "---")
#'
#' @name na_string
NULL

#' @rdname na_string
#' @export
na_string <- function(ht) .prop_get(ht, "na_string")

#' @rdname na_string
#' @export
`na_string<-` <- function(ht, value) {
  .prop_replace(ht, value, "na_string", check_fun = is.character)
}

#' @rdname na_string
#' @export
set_na_string <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "na_string", check_fun = is.character)
}

#' @rdname na_string
#' @export
map_na_string <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "na_string", check_fun = is.character)
}


#' Make cell text bold or italic
#'
#' @inheritParams .hux_prop_params
#' @param value A logical vector or matrix. `r .rd_default("bold")`
#'
#' @family formatting functions
#'
#' @examples
#' bold(jams) <- TRUE
#' bold(jams)
#'
#' set_bold(jams, FALSE)
#' set_bold(
#'   jams,
#'   2:3, 1, FALSE
#' )
#' map_bold(
#'   jams,
#'   by_rows(FALSE, TRUE)
#' )
#'
#' @name bold
NULL

#' @rdname bold
#' @export
bold <- function(ht) .prop_get(ht, "bold")

#' @rdname bold
#' @export
`bold<-` <- function(ht, value) {
  .prop_replace(ht, value, "bold", check_fun = is.logical)
}

#' @rdname bold
#' @export
set_bold <- function(ht, row, col, value = TRUE) {
  .prop_set(ht, row, col, value, "bold", check_fun = is.logical)
}

#' @rdname bold
#' @export
map_bold <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "bold", check_fun = is.logical)
}

#' @rdname bold
#' @export
italic <- function(ht) .prop_get(ht, "italic")

#' @rdname bold
#' @export
`italic<-` <- function(ht, value) {
  .prop_replace(ht, value, "italic", check_fun = is.logical)
}

#' @rdname bold
#' @export
set_italic <- function(ht, row, col, value = TRUE) {
  .prop_set(ht, row, col, value, "italic", check_fun = is.logical)
}

#' @rdname bold
#' @export
map_italic <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "italic", check_fun = is.logical)
}
#' Make text larger or smaller
#'
#' Font size is in points.
#'
#' @inheritParams .hux_prop_params
#' @param value A numeric vector. `r .rd_default("font_size")`
#'
#' @family formatting functions
#'
#' @examples
#' font_size(jams) <- 14
#' font_size(jams)
#'
#' jams2 <- set_font_size(
#'   jams,
#'   12
#' )
#' font_size(jams2)
#'
#' jams3 <- set_font_size(
#'   jams,
#'   2:3, 1, 12
#' )
#' font_size(jams3)
#'
#' jams4 <- map_font_size(
#'   jams,
#'   by_rows(
#'     12,
#'     14
#'   )
#' )
#' font_size(jams4)
#'
#' @name font_size
NULL

#' @rdname font_size
#' @export
font_size <- function(ht) .prop_get(ht, "font_size")

#' @rdname font_size
#' @export
`font_size<-` <- function(ht, value) {
  .prop_replace(ht, value, "font_size", check_fun = is.numeric)
}

#' @rdname font_size
#' @export
set_font_size <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "font_size", check_fun = is.numeric)
}

#' @rdname font_size
#' @export
map_font_size <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "font_size", check_fun = is.numeric)
}


#' Rotate text within cells
#'
#' Numbers represent degrees to rotate text anti-clockwise:
#'
#' * 0 is the default;
#' * 90 is going upwards, for left-to-right languages;
#' * 270 is going downwards.
#'
#' @inheritParams .hux_prop_params
#' @param value A numeric vector or matrix. `r .rd_default("rotation")`
#'
#' @details
#' You will probably need to set [col_width()] and [row_height()] explicitly
#' to achieve a nice result, in both HTML and LaTeX.
#'
#' @examples
#' rotation(jams) <- 90
#' rotation(jams)
#'
#' jams2 <- set_rotation(
#'   jams,
#'   270
#' )
#' rotation(jams2)
#'
#' jams3 <- set_rotation(
#'   jams,
#'   2:3, 1, 270
#' )
#' rotation(jams3)
#'
#' jams4 <- map_rotation(
#'   jams,
#'   by_rows(
#'     270,
#'     90
#'   )
#' )
#' rotation(jams4)
#'
#' @name rotation
NULL

#' @rdname rotation
#' @export
rotation <- function(ht) .prop_get(ht, "rotation")

#' @rdname rotation
#' @export
`rotation<-` <- function(ht, value) {
  .prop_replace(ht, value, "rotation",
    check_fun = is.numeric,
    extra = quote(value <- value %% 360)
  )
}

#' @rdname rotation
#' @export
set_rotation <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "rotation",
    check_fun = is.numeric,
    extra = quote(value <- value %% 360)
  )
}

#' @rdname rotation
#' @export
map_rotation <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "rotation",
    check_fun = is.numeric,
    extra = quote(value <- value %% 360)
  )
}


#' Set how numbers are formatted in cells
#'
#' If `number_format` is:
#' * numeric, numbers will be rounded to that many decimal places;
#' * character, it will be used as an argument to [sprintf()];
#' * a function, the function will be applied to the numbers;
#' * `NA`, then numbers will not be formatted (except by conversion with
#'   `as.character`).
#'
#' @inheritParams .hux_prop_params
#' @param value A character or integer vector,
#'   a list containing a function, or `NA`. Note that setting to `NA` does not reset to the default.
#'
#' @details
#' Number formatting is applied to any parts of cells that look like numbers.
#' The exception is exponents in
#' scientific notation; huxtable attempts to detect and ignore these.
#'
#' The default value is "%.3g", which rounds numbers if they have more than 3
#' significant digits, and which may use scientific notation for large numbers.
#'
#' Note that if your cells are of type numeric, a number format of `NA` doesn't
#' guarantee you get back what you typed in, since R's default conversion may
#' apply scientific notation and rounding.
#'
#' To set number_format to a function, enclose the function in `list`. The function should
#' take one argument and return a string. [fmt_pretty()] and [fmt_percent()]
#' are useful shortcuts for common formatting functions.
#'
#' @family formatting functions
#' @seealso [fmt_pretty()] and [fmt_percent()].`options("huxtable.long_minus")`
#' in [huxtable-options] for pretty-printing minus signs.
#'
#' @examples
#' ht <- huxtable(
#'   number_format = c(
#'     "Default",
#'     "NA",
#'     "2",
#'     "\"%5.2f\"",
#'     "Pretty",
#'     "Sign"
#'   ),
#'   a = rep(1000, 6),
#'   b = rep(1000.005, 6),
#'   c = rep(0.0001, 6),
#'   d = rep(-1, 6),
#'   e = rep("3.2 (s.e. 1.4)", 6)
#' )
#'
#' number_format(ht)[3, -1] <- NA
#' number_format(ht)[4, -1] <- 2
#' number_format(ht)[5, -1] <- "%5.2f"
#'
#' number_format(ht)[6, -1] <- fmt_pretty()
#'
#' number_format(ht)[7, -1] <- list(
#'   function(x) if (x > 0) "+" else "-"
#' )
#'
#' right_border(ht) <- 1
#' bottom_border(ht)[1, ] <- 1
#'
#' ht
#'
#' ht_bands <- huxtable("10000 Maniacs", autoformat = FALSE)
#' # probably not what you want:
#' ht_bands
#' # fixed:
#' set_number_format(ht_bands, NA)
#'
#' @name number_format
NULL
#' @noRd
check_number_format <- function(x) {
  value_ok <- function(y) {
    is.numeric(y) || is.character(y) || is.function(y) || is.na(y)
  }
  all(vapply(x, value_ok, logical(1)))
}

#' @rdname number_format
#' @export
number_format <- function(ht) .prop_get(ht, "number_format")

#' @rdname number_format
#' @export
`number_format<-` <- function(ht, value) {
  .prop_replace(ht, value, "number_format",
    check_fun = check_number_format,
    reset_na = FALSE,
    coerce_mode = FALSE
  )
}

#' @rdname number_format
#' @export
set_number_format <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "number_format",
    check_fun = check_number_format,
    reset_na = FALSE
  )
}

#' @rdname number_format
#' @export
map_number_format <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "number_format",
    check_fun = check_number_format,
    reset_na = FALSE
  )
}


#' Set cell contents
#'
#' `set_contents()` is a convenience function to change the cell contents of a huxtable within
#' a dplyr chain. `set_contents(ht, x, y, foo)` just calls `ht[x, y] <- foo` and returns `ht`.
#'
#' @inheritParams .hux_prop_params
#' @param value Cell contents.
#'
#' @name set_contents
#' @examples
#' data(jams)
#' set_contents(jams, 2, 1, "Blackcurrant")
#' map_contents(jams, by_regex(".*berry" = "Snodberry"))
NULL

#' @rdname set_contents
#' @export
contents <- function(ht) ht

#' @rdname set_contents
#' @export
`contents<-` <- function(ht, value) {
  value
}

#' @rdname set_contents
#' @export
set_contents <- function(ht, row, col, value) {
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  rcrow <- get_rc_spec(ht, row, 1)
  rccol <- get_rc_spec(ht, col, 2)
  ht[rcrow, rccol] <- value
  ht
}

#' @rdname set_contents
#' @export
map_contents <- function(ht, row, col, fn) {
  if (missing(col) && missing(fn)) {
    fn <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  rcrow <- get_rc_spec(ht, row, 1)
  rccol <- get_rc_spec(ht, col, 2)
  current <- ht[rcrow, rccol, drop = FALSE]
  if (is_huxtable(current)) current <- as.matrix(current)
  ht[rcrow, rccol] <- fn(ht, rcrow, rccol, current)
  ht
}


#' Set the font for cell text
#'
#' @inheritParams .hux_prop_params
#' @param value A character vector or matrix. `r .rd_default("font")`
#'
#' @details
#' To find out what fonts are on your system, `systemfonts::match_font()`
#' is useful.
#'
#' For HTML, you can use comma-separated lists of font names like
#' `"Times New Roman, Times, Serif"`. This is not portable, though.
#'
#' LaTeX and HTML use different font names. To use the same font
#' names across document formats, see `options("huxtable.latex_use_fontspec")`
#' in [huxtable-options].
#'
#' @family formatting functions
#'
#' @examples
#' font(jams) <- "times"
#' font(jams)
#'
#' set_font(jams, "arial")
#' set_font(jams, 2:3, 1, "arial")
#' map_font(jams, by_rows("arial", "times"))
#'
#' @name font
NULL

#' @rdname font
#' @export
font <- function(ht) .prop_get(ht, "font")

#' @rdname font
#' @export
`font<-` <- function(ht, value) {
  .prop_replace(ht, value, "font", check_fun = is.character)
}

#' @rdname font
#' @export
set_font <- function(ht, row, col, value) {
  .prop_set(ht, row, col, value, "font", check_fun = is.character)
}

#' @rdname font
#' @export
map_font <- function(ht, row, col, fn) {
  .prop_map(ht, row, col, fn, "font", check_fun = is.character)
}
