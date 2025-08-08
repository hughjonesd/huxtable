#' Set the width of table columns
#'
#' Numeric column widths are treated as proportions of the table width.
#' Character widths must be valid CSS or LaTeX dimensions.
#'
#' @param ht A huxtable.
#' @param col A column specifier. See [rowspecs] for details.
#' @param value Numeric or character vector. Set to `NA` to reset to the default,
#'   which is `r huxtable::get_default_properties("col_width")[[1]]`.
#'
#' @details
#' In LaTeX, if you specify a column width, but set `wrap` to `FALSE` and have
#' cells which overrun, then you may have problems with table position and with
#' background colours in other cells. The workaround is to adjust the width, so
#' that your cells no longer overrun.
#'
#' @return `col_width()` returns the `col_width` property.
#' `set_col_width()` returns the modified huxtable.
#' @family table measurements
#'
#' @examples
#' col_width(jams) <- c(.2, .8)
#' col_width(jams)
#'
#' jams$Notes <- c(
#'   "Notes",
#'   "This year's finest", "", ""
#' )
#' jams
#' set_col_width(jams, c(.4, .5, .1))
#'
#' @aliases col_width<- set_col_width
#' @name col_width
NULL

#' @rdname col_width
#' @export
col_width <- function(ht) .prop_get(ht, "col_width")

#' @rdname col_width
#' @export
`col_width<-` <- function(ht, value) {
  .prop_replace(ht, value, "col_width", check_fun = is_numeric_or_character)
}

#' @rdname col_width
#' @export
set_col_width <- function(ht, col, value) {
  .prop_set_col(ht, col, value, "col_width", check_fun = is_numeric_or_character)
}

map_col_width <- function(ht, col, fn) {
  .prop_map_col(ht, col, fn, "col_width", check_fun = is_numeric_or_character)
}


#' Set the height of table rows
#'
#' Numeric heights are scaled to 1 and treated as proportions of the table height
#' in HTML, or of the text height (`\textheight`) in LaTeX. Character
#' row heights must be valid CSS or LaTeX dimensions.
#'
#' @inheritParams col_width
#' @param row A row specifier. See [rowspecs] for details.
#' @param value Numeric or character vector. Set to `NA` to reset to the default,
#'   which is `r huxtable::get_default_properties("row_height")[[1]]`.
#' @return `row_height()` returns the `row_height` property.
#' `set_row_height()` returns the modified huxtable.
#' @family table measurements
#'
#' @examples
#' row_height(jams) <- c(.4, .2, .2, .2)
#' row_height(jams)
#'
#' @seealso [col_width()], [height()], [width()]
#' @aliases row_height<- set_row_height
#' @name row_height
NULL

#' @rdname row_height
#' @export
row_height <- function(ht) .prop_get(ht, "row_height")

#' @rdname row_height
#' @export
`row_height<-` <- function(ht, value) {
  .prop_replace(ht, value, "row_height", check_fun = is_numeric_or_character)
}

#' @rdname row_height
#' @export
set_row_height <- function(ht, row, value) {
  .prop_set_row(ht, row, value, "row_height", check_fun = is_numeric_or_character)
}

map_row_height <- function(ht, row, fn) {
  .prop_map_row(ht, row, fn, "row_height", check_fun = is_numeric_or_character)
}


#' Mark rows or columns as headers
#'
#' Arbitrary rows and columns can be headers: they do not have to be at the top
#' or left of the table.
#'
#' @inheritParams col_width
#' @param value Logical vector. Set to `NA` to reset to the default,
#'   which is `r huxtable::get_default_properties("header_cols")[[1]]`.
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
#'   bold       = TRUE,
#'   text_color = "purple"
#' )
#'
#' @aliases header_cols<- set_header_cols
#' @name header_cols
NULL

#' @rdname header_cols
#' @export
header_cols <- function(ht) .prop_get(ht, "header_cols")

#' @rdname header_cols
#' @export
`header_cols<-` <- function(ht, value) {
  .prop_replace(ht, value, "header_cols", check_fun = is.logical)
}

#' @rdname header_cols
#' @export
set_header_cols <- function(ht, col, value) {
  .prop_set_col(ht, col, value, "header_cols", check_fun = is.logical)
}

map_header_cols <- function(ht, col, fn) {
  .prop_map_col(ht, col, fn, "header_cols", check_fun = is.logical)
}



#' @rdname header_cols
#' @inheritParams header_cols
#' @param row A row specifier. See [rowspecs] for details.
#' @aliases header_rows header_rows<- set_header_rows
#' @name header_rows
NULL

#' @rdname header_cols
#' @export
header_rows <- function(ht) .prop_get(ht, "header_rows")

#' @rdname header_cols
#' @export
`header_rows<-` <- function(ht, value) {
  .prop_replace(ht, value, "header_rows", check_fun = is.logical)
}

#' @rdname header_cols
#' @export
set_header_rows <- function(ht, row, value) {
  .prop_set_row(ht, row, value, "header_rows", check_fun = is.logical)
}

map_header_rows <- function(ht, row, fn) {
  .prop_map_row(ht, row, fn, "header_rows", check_fun = is.logical)
}
