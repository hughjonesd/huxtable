
#' Set the width of table columns
#'
#' Numeric column widths are treated as proportions of the table width.
#' Character widths must be valid CSS or LaTeX dimensions.
#'
#' @template getset-rowcol
#' @templateVar attr_name col_width
#' @templateVar rowcol col
#' @templateVar value_param_desc Numeric or character vector.
#'
#' @details
#' In LaTeX, if you specify a column width, but set `wrap` to `FALSE` and have
#' cells which overrun, then you may have problems with table position and with
#' background colours in other cells. The workaround is to adjust the width, so
#' that your cells no longer overrun.
#'
#' @family table measurements
#' @template getset-example
#' @templateVar attr_val c(.2, .8)
#'
#' @examples
#' jams$Notes <- c("Notes",
#'       "This year's finest", "", "")
#' jams
#' set_col_width(jams, c(.4, .5, .1))
NULL
make_getter_setters("col_width", "col", check_fun = is_numeric_or_character)


#' Set the height of table rows
#'
#' Numeric heights are scaled to 1 and treated as proportions of the table height
#' in HTML, or of the text height (`\\textheight`) in LaTeX. Character
#' row heights must be valid CSS or LaTeX dimensions.
#'
#' @template getset-rowcol
#' @templateVar attr_name row_height
#' @templateVar rowcol row
#' @templateVar value_param_desc Numeric or character vector.
#'
#' @family table measurements
#'
#' @template getset-example
#' @templateVar attr_val c(.4, .2, .2, .2)
NULL
make_getter_setters("row_height", "row", check_fun = is_numeric_or_character)


#' Mark rows or columns as headers
#'
#' Arbitrary rows and columns can be headers: they do not have to be at the top
#' or left of the table.
#'
#' @template getset-rowcol
#' @templateVar attr_name header_cols
#' @templateVar rowcol col
#' @templateVar value_param_desc Logical vector
#'
#' @details
#' By default header rows and columns are not shown differently from other rows, but
#' you can change this with [style_headers()].
#' Various themes may set properties on headers. Lastly, headers are treated
#' differently when [restacking][restack-across-down].
#'
#' @examples
#'
#' jams <- set_header_rows(jams, 1, TRUE)
#' jams <- set_header_cols(jams, 1, TRUE)
#' style_headers(jams,
#'        bold       = TRUE,
#'        text_color = "purple"
#'      )
#'
NULL
make_getter_setters("header_cols", "col", check_fun = is.logical)



#' @name header_rows
#' @param row A row specifier. See [rowspecs] for details.
#' @usage
#' header_rows(ht)
#' header_rows(ht) <- value
#' set_header_rows(ht, row, value)
#' @rdname header_cols
#' @aliases header_rows header_rows<- set_header_rows
NULL
make_getter_setters("header_rows", "row", check_fun = is.logical)
