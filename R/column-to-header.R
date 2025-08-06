#' Convert a column to header rows
#'
#' @inherit left_border params return
#' @param col A column specifier for a single column.
#' @param ... Properties to set on new rows
#' @param glue Glue string. `"{value}"` will be replaced by the column value.
#' @param start_col Integer. New header text will start at this column.
#' @param ignore_headers Logical. Ignore existing headers?
#' @param set_headers Logical. Set new rows as headers?
#'
#' @export
#'
#' @examples
#' column_to_header(jams, "Type")
#' column_to_header(jams, "Type", text_color = "red")
#' column_to_header(jams, "Price",
#'   number_format = 2,
#'   italic = TRUE,
#'   glue = "Price: {value}"
#' )
#'
#' iris_hux <- as_hux(iris[c(1:4, 51:54, 101:104), ])
#' column_to_header(iris_hux, "Species",
#'   markdown = TRUE,
#'   glue = "Species: **{value}**"
#' )
column_to_header <- function(ht, col, ..., glue = "{value}", start_col = 1,
                             ignore_headers = TRUE, set_headers = TRUE) {
  assert_that(
    is_huxtable(ht), is.count(start_col), is.string(glue),
    is.flag(ignore_headers), is.flag(set_headers)
  )

  col <- get_rc_spec(ht, col, 2)
  if (is.logical(col)) col <- which(col)
  if (is.character(col)) col <- which(colnames(ht) == col)
  assert_that(length(col) == 1L)

  col_values <- ht[[col]]

  if (length(col_values) < 1L) {
    return(ht)
  }

  repeated <- col_values[-1] == col_values[-length(col_values)]
  repeated <- c(FALSE, repeated)
  if (ignore_headers) {
    headers <- header_rows(ht)
    repeated <- repeated | headers
  }
  changes <- which(!repeated)

  # we do this in reverse order so we don't need to recalculate the row numbers
  for (change in rev(changes)) {
    value <- col_values[change]
    value <- glue::glue(glue, value = value)
    start_cells <- rep("", start_col - 1)
    ht <- insert_row(ht, start_cells, value,
      after = change - 1, fill = "",
      copy_cell_props = FALSE
    )
    if (length(start_cells) > 0L) colspan(ht)[change, 1] <- length(start_cells)
    colspan(ht)[change, start_col] <- ncol(ht) - start_col + 1
    if (length(list(...))) {
      ht <- style_cells(ht, change, start_col:ncol(ht), ...)
    }
    if (set_headers) ht <- set_header_rows(ht, change, TRUE)
  }

  ht <- ht[-col]

  ht
}
