
#' Merge a range of cells
#'
#' @param ht A huxtable.
#' @param row A row specifier. See \code{\link{rowspecs}} for details. Only the minimum and maximum
#' rows and columns are used.
#' @param col A column specifier.
#'
#' @details
#' `merge_cells(ht, c(min_row, max_row), c(min_col, max_col))` is equivalent to
#' ```
#'   colspan(ht)[min_row, min_col] <- max_col - min_col + 1
#'   rowspan(ht)[min_row, min_col] <- max_row - min_row + 1
#' ```
#' @return The `ht` object.
#'
#' @family cell merging
#'
#' @export
#' @examples
#'
#' ht <- hux(a = 1:3, b = 1:3)
#' ht <- set_all_borders(ht, 1)
#' merge_cells(ht, 2:3, 1:2)
#'
merge_cells <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  mr <- min(row)
  mc <- min(col)
  cs <- diff(range(col)) + 1
  rs <- diff(range(row)) + 1
  colspan(ht)[mr, mc] <- cs
  rowspan(ht)[mr, mc] <- rs

  ht
}


#' Merge cells across rows or down columns
#'
#' `merge_across` creates multicolumn cells within each row. `merge_down` creates
#' multirow cells within each column.
#'
#' @inherit left_border params
#'
#' @return The `ht` object.
#'
#' @export
#'
#' @family cell merging
#'
#' @examples
#'
#' ht <- as_hux(matrix(1:12, 4, 3, byrow = TRUE))
#' ht <- set_all_borders(ht, 1)
#' merge_across(ht, 2:4, 2:3)
#' merge_down(ht, 2:4, 2:3)
#'
merge_across <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  for (r in row) ht <- merge_cells(ht, r, col)

  ht
}


#' @rdname merge_across
#' @export
merge_down <- function (ht, row, col) {
  assert_that(is_huxtable(ht))

  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  for (cl in col) ht <- merge_cells(ht, row, cl)

  ht
}


#' Merge repeated rows into multirow cells
#'
#' @param ht A huxtable.
#' @param row A row specification.
#' @param col A column specification.
#'
#' @return The modified huxtable.
#' @export
#'
#' @details
#' Repeated rows in each column are merged into cells with
#' `rowspan > 1`.
#'
#' If `row` contains gaps, results may be unexpected (and a warning is given).
#'
#' @seealso merge_cells
#'
#' @examples
#' ht <- as_hux(jams[c(1, 2, 2, 3, 3, 4), ])
#' ht <- add_columns(ht, c("Sugar", "30%", "40%", "30%", "40%", "30%"),
#'       after = 1)
#' ht
#' merge_repeated_rows(ht)
#' merge_repeated_rows(ht, everywhere, "Type")
merge_repeated_rows <- function (ht, row, col) {
  assert_that(is_huxtable(ht))
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  if (length(row) > 1L && ! all(row == seq(min(row), max(row)))) {
    warning("Non-contiguous rows: ", paste(row, collapse = ", "))
  }
  for (cc in col) {
    contents <- ht[row, ][[cc]] # gets a vector
    new <- which(c(TRUE, contents[seq_len(length(contents) - 1)] != contents[-1]))
    spans <- diff(c(new, length(contents) + 1))
    rowspan(ht)[row[new], cc] <- spans
  }

  ht
}
