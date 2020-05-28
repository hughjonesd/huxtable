
#' @template getset-cell
#' @templateVar attr_name valign
#' @templateVar attr_desc Vertical alignment
#' @templateVar value_param_desc A character vector or matrix which may be "top", "middle", "bottom" or `NA`.
#' @template getset-example
#' @templateVar attr_val "bottom"
#' @template getset-rowspec-example
#' @templateVar attr_val2 "bottom"
#' @details
#' Vertical alignment may not work for short text in LaTeX. Defining row heights with [row_height()]
#' may help.
NULL
make_getter_setters("valign", "cell", check_fun = is.character, check_values = c("top", "middle", "bottom"))


check_align_value <- function (x) {
  x <- na.omit(x)
  is.character(x) && all(x %in% c("left", "centre", "center", "right") | ncharw(x) == 1)
}


#' @template getset-cell
#' @templateVar attr_name align
#' @templateVar attr_desc Alignment
#' @templateVar value_param_desc A character vector or matrix which may be "left", "center", "right" , `NA` or a single character.
#' @template getset-example
#' @templateVar attr_val "right"
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 "left"
#' @details This sets the horizontal alignment of the cell. If `value` is a single character (e.g.
#' a decimal point), then the cell is aligned on this character.
NULL
make_getter_setters("align", "cell", check_fun = check_align_value,
  extra_code = value[value == "centre"] <- "center")


#' @template getset-cell
#' @templateVar attr_name background_color
#' @templateVar attr_desc Background color
#' @templateVar value_param_desc A character vector or matrix of valid R color names.
#' @template getset-example
#' @templateVar attr_val grey(.95)
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 "yellow"
#' @family formatting functions
NULL
make_getter_setters("background_color", "cell")


#' @template getset-cell
#' @templateVar attr_name text_color
#' @templateVar attr_desc Text color
#' @templateVar value_param_desc A character vector or matrix of valid R colors.
#' @details
#' Colors can be in any format understood by R, e.g. `"red"`, `"#FF0000"` or `rgb(1, 0, 0)`.
#' @template getset-example
#' @templateVar attr_val "blue"
#' @template getset-visible-rowspec-example
#' @templateVar attr_val2 "red"
#' @family formatting functions
NULL
make_getter_setters("text_color", "cell")


#' @template getset-cell
#' @templateVar attr_name wrap
#' @templateVar attr_desc Text wrapping
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, long cell contents will be wrapped into multiple lines.
#' Set to `NA` for the default.
#' @examples
#' ht <- huxtable(paste(
#'       rep("Some long text.", 20),
#'       collapse = " "))
#' width(ht) <- 0.2
#' wrap(ht) <- TRUE
#' \dontrun{
#'   quick_html(ht)
#' }
#'
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
#'
NULL
make_getter_setters("wrap", "cell", check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name escape_contents
#' @templateVar attr_desc Escape cell contents
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, cell contents will be HTML or LaTeX escaped.
#' @details
#' If [markdown()] is `TRUE` for a cell, the `escape_contents` property
#' will be ignored.
#'
#' @examples
#' ht <- huxtable(
#'         Exponent = 2:4,
#'         Example  = paste0("$x^", 2:4, "$")
#'       )
#' escape_contents(ht)[,2] <- FALSE
#' \dontrun{
#'   quick_pdf(ht)
#' }
#'
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
NULL
make_getter_setters("escape_contents", "cell", check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name markdown
#' @templateVar attr_desc markdown
#' @templateVar value_param_desc
#' A logical vector or matrix. If `TRUE`, cell contents will be rendered as [markdown](https://commonmark.org/help/).
#' @template getset-rowspec-example
#' @templateVar attr_val TRUE
#' @templateVar attr_val2 FALSE
#' @templateVar default TRUE
NULL
make_getter_setters("markdown", "cell", check_fun = is.logical, default = TRUE)



#' @template getset-cell
#' @templateVar attr_name na_string
#' @templateVar attr_desc NA string
#' @templateVar value_param_desc
#' A character string. This will be used to replace NA values in the display.
#' @template getset-example
#' @templateVar attr_val "--"
#' @noMd
#' @templateVar extra jams[2,2] <- NA ## jams
#' @template getset-rowspec-example
#' @templateVar attr_val2 ""
#' @family formatting functions
NULL
make_getter_setters("na_string", "cell", check_fun = is.character)


#' @template getset-cell
#' @templateVar attr_name bold
#' @templateVar attr_desc Cell text style
#' @templateVar value_param_desc
#' A logical vector or matrix. `TRUE` for bold/italic.
#' @templateVar morealiases italic
#' @templateVar default TRUE
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
#' @return
#' Similarly for \code{italic} and friends.
NULL
make_getter_setters("italic", "cell", default = TRUE, check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name font_size
#' @templateVar attr_desc Font size
#' @templateVar value_param_desc
#' A numeric vector. This sets the font size in points.
#' @template getset-example
#' @templateVar attr_val 14
#' @template getset-rowspec-example
#' @templateVar attr_val2 12
#' @family formatting functions
NULL
make_getter_setters("font_size", "cell", check_fun = is.numeric)


#' @template getset-cell
#' @templateVar attr_name rotation
#' @templateVar attr_desc Text rotation
#' @templateVar value_param_desc
#' A numeric vector. Anti-clockwise from the x axis, so 0 is left to right, 90 is going up, etc.
#' @template getset-example
#' @templateVar attr_val 90
#' @template getset-rowspec-example
#' @templateVar attr_val2 270
#' @details
#' You will probably need to set [col_width()] and [row_height()] explicitly
#' to achieve a nice result, in both HTML and LaTeX.
NULL
make_getter_setters("rotation", "cell", check_fun = is.numeric, extra_code = {value <- value %% 360})


#' @template getset-cell
#' @templateVar attr_name number_format
#' @templateVar attr_desc Number format
#' @templateVar value_param_desc A character or integer vector, or a list containing a function, or \code{NA}.
#' @templateVar NA_does_not_reset TRUE
#' @details
#' Number formatting is applied to any parts of cells that look like numbers (defined as an optional minus sign,
#' followed by
#' numerals, followed by an optional decimal point and further numerals). The exception is exponents in
#' scientific notation; huxtable attempts to detect and ignore these.
#'
#' If `value` is
#' * numeric, numbers will be rounded to that many decimal places;
#' * character, it will be taken as an argument to [sprintf()];
#' * a function, the function will be applied to the numbers;
#' * `NA`, then numbers will not be formatted (except maybe by conversion with `as.character`).
#'
#' Note that if your cells are of type numeric, a number format of `NA` doesn't guarantee you get
#' back what you typed in, since R's default conversion may apply scientific notation and
#' rounding.
#'
#' The default value is "\%.3g", which rounds numbers if they have more than 3 significant
#' digits, and which may use scientific notation for large numbers.
#'
#' To set number_format to a function, enclose the function in `list`. The function should
#' take one argument and return a string. [fmt_pretty()] and [fmt_percent()]
#' are useful shortcuts for common formatting functions.
#'
#' Versions of huxtable before 2.0.0 applied `number_format` only to cells that looked like
#' numbers in their entirety. The default value was "\%5.2f".
#'
#' @family formatting functions
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
#' @template getset-visible-rowspec-example
#' @templateVar attr_val 2
#' @templateVar attr_val2 3
NULL
make_getter_setters("number_format", "cell")


# override the default
`number_format<-.huxtable` <- function(ht, value) {
  stopifnot(all(sapply(value, function (x) is.numeric(x) || is.character(x) || is.function(x) || is.na(x) )))
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
#' @param ht A huxtable.
#' @param value Cell contents.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param fn A mapping function. See [mapping-functions] for details.
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
  value # by the time we get here, the replacement has already happened and we're done
}


#' @template getset-cell
#' @templateVar attr_name font
#' @templateVar attr_desc Font
#' @templateVar value_param_desc
#' A character vector of font names.
#' @details
#' LaTeX and HTML use different font names. If you want to use the same font
#' names across document formats, set `options("huxtable.latex_use_fontspec")`
#' to `TRUE`. See [huxtable-options].
#'
#' @template getset-example
#' @templateVar attr_val "times"
#' @template getset-rowspec-example
#' @templateVar attr_val2 "arial"
#' @family formatting functions
#' @template cell-property-usage
NULL
make_getter_setters("font", "cell", check_fun = is.character)
