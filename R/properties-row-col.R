
#' @template getset-rowcol
#' @templateVar attr_name col_width
#' @templateVar rowcol col
#' @templateVar attr_desc Column width
#' @templateVar value_param_desc A vector. If numeric, they are treated as proportions of the table width. If character, they must be valid CSS or LaTeX lengths.
#' @details
#' In LaTeX, if you specify a column width, but set `wrap` to `FALSE` and have cells which
#' overrun, then you may have problems with table position and with background colours in other cells.
#' The workaround is to adjust the width, so that your cells no longer overrun.
#' @family row/column sizes
#' @template getset-example
#' @templateVar attr_val c(.2, .8)
NULL
make_getter_setters("col_width", "col")


#' @name header-details
#' @details
#' Arbitrary rows and columns can be headers - they do not have to be at the top
#' or left of the table.
#' By default header rows and columns are not shown differently from other rows, but
#' you can change this with [style_headers()].
#' Various themes also set properties on headers.
NULL


#' @template getset-rowcol
#' @templateVar attr_name row_height
#' @templateVar rowcol row
#' @templateVar attr_desc Row height
#' @templateVar value_param_desc A vector.
#' @family row/column sizes
#' @details
#' If character, `value` must contain valid CSS or LaTeX lengths. If numeric, in HTML, values are scaled to 1 and treated as proportions of the table height. In LaTeX, they are
#' treated as proportions of the text height (`\\textheight`).
#' @template getset-example
#' @templateVar attr_val c(.2, .1, .1, .1)
NULL
make_getter_setters("row_height", "row")


#' @template getset-rowcol
#' @templateVar attr_name header_cols
#' @templateVar rowcol col
#' @templateVar attr_desc header column
#' @templateVar value_param_desc A logical vector.
#' @inherit header-details details
#' @family header cells
#' @template getset-example
#' @templateVar attr_val c(TRUE, FALSE)
NULL
make_getter_setters("header_cols", "col", check_fun = is.logical)


#' @template getset-rowcol
#' @templateVar attr_name header_rows
#' @templateVar rowcol row
#' @templateVar attr_desc header row
#' @templateVar value_param_desc A logical vector.
#' @inherit header-details details
#' @family header cells
#' @template getset-example
#' @templateVar attr_val c(TRUE, FALSE, FALSE, FALSE)
NULL
make_getter_setters("header_rows", "row", check_fun = is.logical)


#' @template getset-cell
#' @templateVar attr_name rowspan
#' @templateVar attr_desc Row and column span
#' @templateVar value_param_desc An integer vector or matrix.
#' @details
#' The rowspan and colspan of a cell determine its height and width, in rows and columns.
#' A cell with rowspan of 2 covers the cell directly below it. A cell with rowspan of 2
#' and colspan of 2 covers a 2 x 2 square, hiding three other cells.
#' @template getset-example
#' @noMd
#' @templateVar subscript [1, 1]
#' @templateVar attr_val 2
#' @templateVar extra jams <- set_all_borders(jams, 1) ## jams
NULL
make_getter_setters("rowspan", "cell", check_fun = is.numeric, extra_code = {
  if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop("rowspan would extend beyond bottom of table")
  # throws an error if cells are cut
  display_cells(ht, new_rowspan = value)
}
)


#' @name colspan
#' @rdname rowspan
#' @templateVar attr_name colspan
#' @template cell-property-usage
#' @aliases colspan<- set_colspan map_colspan
NULL
make_getter_setters("colspan", "cell", check_fun = is.numeric, extra_code = {
  if (any(na.omit( col(ht) + value - 1 > ncol(ht) ))) stop(
    "colspan would extend beyond right edge of table")
  # throws an error if cells are cut
  display_cells(ht, new_colspan = value)
}
)

