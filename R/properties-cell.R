
#' Set the vertical alignment of cell content
#'
#' Allowed values are "top", "middle", "bottom" or `NA`.
#'
#' @template getset-cell
#' @templateVar attr_name valign
#' @templateVar value_param_desc A character vector or matrix.
#'
#' @details
#' Vertical alignment may not work for short text in LaTeX.
#' Defining row heights with [row_height()] may help.
#'
#' @template getset-example
#' @templateVar attr_val "top"
#' @template getset-rowspec-example
#' @templateVar attr_val2 "bottom"
NULL
make_getter_setters("valign", "cell",
        check_fun = is.character,
        check_values = c("top", "middle", "bottom")
      )


check_align_value <- function (x) {
  x <- na.omit(x)
  is.character(x) && all(x %in% c("left", "centre", "center", "right") | ncharw(x) == 1)
}


#' Set the horizontal alignment of cell content
#'
#' Values may be "left", "center", "right", `NA` or a single character. If
#' `value` is a single character (e.g. a decimal point), then the cell is
#' aligned on this character.
#'
#' @template getset-cell
#' @templateVar attr_name align
#' @templateVar value_param_desc A character vector or matrix.
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
#'                                     decimal.mark = ",",
#'                                     big.mark = ""
#'                                   )
#'
#' number_hux <- map_align(number_hux,
#'       by_cols("left", "center", "right", ".", ","))
#'
#' alignments <- c(
#'                  "left",
#'                  "centre",
#'                  "right",
#'                  "decimal (.)",
#'                  "decimal (,)"
#'                )
#' number_hux <- rbind(
#'         alignments,
#'         number_hux
#'       )
#'
#' align(number_hux)
#' number_hux
#'
#'
NULL
make_getter_setters("align", "cell",
        check_fun  = check_align_value,
        extra_code = {
          value[value == "centre"] <- "center"
        }
      )


#' Extend cells over multiple rows and/or columns
#'
#' A cell with rowspan of 2 covers the cell directly below it. A cell with
#' colspan of 2 covers the cell directly to its right. A cell with rowspan of 2
#' and colspan of 2 covers a 2 x 2 square, hiding three other cells.
#'
#' @template property-params
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


#' @name rowspan
#' @rdname spans
#' @template cell-property-usage
#' @templateVar attr_name rowspan
#' @aliases rowspan<- set_rowspan map_rowspan
NULL
make_getter_setters("rowspan", "cell",
        check_fun = is.numeric,
        extra_code = {
          too_long <- na.omit(row(ht) + value - 1 > nrow(ht))
          if (any(too_long)) {
            stop("rowspan would extend beyond bottom of table")
          }
          # throws an error if cells are cut
          dc <- display_cells(ht, new_rowspan = value)
          if (any(value > 1)) {
            ht <- overwrite_shadowed_cells(ht, dc)
          }
        }
      )


#' @name colspan
#' @rdname spans
#' @template cell-property-usage
#' @templateVar attr_name colspan
#' @aliases colspan<- set_colspan map_colspan
NULL
make_getter_setters("colspan", "cell",
        check_fun = is.numeric,
        extra_code = {
          too_long <- na.omit(col(ht) + value - 1 > ncol(ht))
          if (any(too_long)) {
            stop("colspan would extend beyond right edge of table")
          }
          # throws an error if cells are cut
          dc <- display_cells(ht, new_colspan = value)
          if (any(value > 1)) {
            ht <- overwrite_shadowed_cells(ht, dc)
          }
        }
      )


overwrite_shadowed_cells <- function (ht, dc) {
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
#' @template getset-cell
#' @templateVar attr_name background_color
#' @templateVar value_param_desc A character vector or matrix.
#'
#' @details
#' Transparent colors are not guaranteed to work at present.
#'
#' @family formatting functions
#'
#' @template getset-example
#' @templateVar attr_val grey(0.7)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 "yellow"
NULL
make_getter_setters("background_color", "cell")


#' Set the color of text in cells
#'
#' @inherit description-colors description
#'
#' @template getset-cell
#' @templateVar attr_name text_color
#' @templateVar value_param_desc A character vector or matrix.
#'
#' @family formatting functions
#'
#' @template getset-example
#' @templateVar attr_val "blue"
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 "red"
NULL
make_getter_setters("text_color", "cell")


#' Wrap cell content over multiple lines
#'
#' Text wrapping only really makes sense when the table [width()] has been set.
#'
#' @template getset-cell
#' @templateVar attr_name wrap
#' @templateVar value_param_desc A logical vector or matrix.
#'
#' @examples
#'
#' long_text <- paste(
#'         rep("Some long text.", 10),
#'         collapse = " "
#'      )
#' ht <- huxtable(Long = long_text)
#' width(ht) <- 0.2
#' wrap(ht) <- TRUE
#'
#' \dontrun{
#'   quick_html(ht)
#' }
#'
NULL
make_getter_setters("wrap", "cell", check_fun = is.logical)


#' Escape or unescape text in cells
#'
#' Setting `escape_contents` to `FALSE` allows you to include raw HTML or
#' TeX code in your cells.
#'
#' @template getset-cell
#' @templateVar attr_name escape_contents
#' @templateVar value_param_desc A logical vector or matrix.
#'
#' @details
#' If [markdown()] is `TRUE` for a cell, the `escape_contents` property
#' will be ignored.
#'
#' @seealso [sanitize()] for escaping text manually.
#'
#' @examples
#'
#' ht <- huxtable(
#'         Text   = "x squared",
#'         Maths  = "$x^2$"
#'       )
#' ht <- set_escape_contents(ht, FALSE)
#' \dontrun{
#'   quick_pdf(ht)
#' }
#'
NULL
make_getter_setters("escape_contents", "cell", check_fun = is.logical)


#' Interpret cell content as markdown
#'
#' Cells where the markdown property is `TRUE` will be interpreted as
#' [markdown](https://commonmark.org/help/).
#'
#' @template getset-cell
#' @templateVar attr_name markdown
#' @templateVar value_param_desc A logical vector or matrix.
#' @templateVar default TRUE
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
#'
#' jams[3, 2] <- "~2.10~ **Sale!** 1.50"
#' set_markdown(jams, 3, 2)
#'
NULL
make_getter_setters("markdown", "cell",
        check_fun = is.logical,
        default   = TRUE
      )



#' Change how NA values are printed
#'
#' NA values in the huxtable are printed as the value of `na_string`.
#'
#' @template getset-cell
#' @templateVar attr_name na_string
#' @templateVar value_param_desc A character vector or matrix.
#'
#' @family formatting functions
#'
#' @examples
#'
#' jams[3, 2] <- NA
#' jams
#' set_na_string(jams, "---")
#'
NULL
make_getter_setters("na_string", "cell", check_fun = is.character)


#' Make cell text bold or italic
#'
#' @template getset-cell
#' @templateVar attr_name bold
#' @templateVar value_param_desc A logical vector or matrix.
#' @templateVar morealiases italic
#' @templateVar default TRUE
#'
#' @template getset-example
#' @templateVar attr_val TRUE
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 FALSE
#' @family formatting functions
NULL
make_getter_setters("bold", "cell", default = TRUE, check_fun = is.logical)


#' @name italic
#' @rdname bold
#' @templateVar attr_name italic
#' @templateVar default TRUE
#' @template cell-property-usage
NULL
make_getter_setters("italic", "cell", default = TRUE, check_fun = is.logical)


#' Make text larger or smaller
#'
#' Font size is in points.
#'
#' @template getset-cell
#' @templateVar attr_name font_size
#' @templateVar attr_desc Font size
#' @templateVar value_param_desc A numeric vector.
#'
#' @template getset-example
#' @templateVar attr_val 14
#' @template getset-rowspec-example
#' @templateVar attr_val2 12
#' @family formatting functions
NULL
make_getter_setters("font_size", "cell", check_fun = is.numeric)


#' Rotate text within cells
#'
#' Numbers represent degrees to rotate text anti-clockwise:
#'
#' * 0 is the default;
#' * 90 is going upwards, for left-to-right languages;
#' * 270 is going downwards.
#'
#' @template getset-cell
#' @templateVar attr_name rotation
#' @templateVar value_param_desc A numeric vector or matrix.
#'
#' @details
#' You will probably need to set [col_width()] and [row_height()] explicitly
#' to achieve a nice result, in both HTML and LaTeX.
#'
#' @template getset-example
#' @templateVar attr_val 90
#' @template getset-rowspec-example
#' @templateVar attr_val2 270
NULL
make_getter_setters("rotation", "cell",
        check_fun = is.numeric,
        extra_code = {value <- value %% 360}
      )


#' Set how numbers are formatted in cells
#'
#' If `number_format` is:
#' * numeric, numbers will be rounded to that many decimal places;
#' * character, it will be used as an argument to [sprintf()];
#' * a function, the function will be applied to the numbers;
#' * `NA`, then numbers will not be formatted (except by conversion with
#'   `as.character`).
#'
#' @template getset-cell
#' @templateVar attr_name number_format
#' @templateVar value_param_desc A character or integer vector,
#'   a list containing a function, or \code{NA}.
#' @templateVar NA_does_not_reset TRUE
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
#'         number_format = c(
#'           "Default",
#'           "NA",
#'           "2",
#'           "\"%5.2f\"",
#'           "Pretty",
#'           "Sign"
#'         ),
#'         a = rep(1000, 6),
#'         b = rep(1000.005, 6),
#'         c = rep(0.0001, 6),
#'         d = rep(-1, 6),
#'         e = rep("3.2 (s.e. 1.4)", 6)
#'       )
#'
#' number_format(ht)[3, -1] <- NA
#' number_format(ht)[4, -1] <- 2
#' number_format(ht)[5, -1] <- "%5.2f"
#'
#' number_format(ht)[6, -1] <- fmt_pretty()
#'
#' number_format(ht)[7, -1] <- list(
#'         function(x) if (x > 0) "+" else "-"
#'       )
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
NULL
make_getter_setters("number_format", "cell")


# override the default
`number_format<-.huxtable` <- function(ht, value) {
  value_ok <- function (x) {
    is.numeric(x) || is.character(x) || is.function(x) || is.na(x)
  }
  stopifnot(all(sapply(value, value_ok)))
  attr(ht, "number_format")[] <- value
  ht
}


#' Set cell contents
#'
#' `set_contents()` is a convenience function to change the cell contents of a huxtable within
#' a dplyr chain. `set_contents(ht, x, y, foo)` just calls `ht[x, y] <- foo` and returns `ht`.
#'
#' @template cell-property-usage
#' @templateVar attr_name contents
#'
#' @template property-params
#' @param value Cell contents.
#'
#' @evalNamespace make_exports("contents", with_map = TRUE)
#' @evalNamespace make_namespace_S3_entries("contents")
#' @aliases contents contents<- map_contents
#' @name set_contents
#' @examples
#'
#' set_contents(jams, 2, 1, "Blackcurrant")
#' map_contents(jams, by_regex(".*berry" = "Snodberry"))
NULL
make_getter_setters("contents", "cell")

#' @evalNamespace "S3method(contents, huxtable)"
contents.huxtable <- function (ht) ht


#' @evalNamespace "S3method(\"contents<-\", huxtable)"
`contents<-.huxtable` <- function (ht, value) {
  value # by the time we get here, the replacement has already happened
}


#' Set the font for cell text
#'
#' @template getset-cell
#' @templateVar attr_name font
#' @templateVar value_param_desc A character vector or matrix.
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
#' @template getset-example
#' @templateVar attr_val "times"
#' @template getset-rowspec-example
#' @templateVar attr_val2 "arial"
NULL
make_getter_setters("font", "cell", check_fun = is.character)
