#' @section Cell content:
#' In merged cell ranges, only the top left cell's content is displayed.
#' In addition, when you merge cells (either by setting [colspan()] or
#' [rowspan()], or using [merge_cells()] and friends) the content of the top
#' left cell is copied to other cells. This prevents unexpected changes to
#' content if you reorder or subset rows and columns.
#'
#'
#' @name span-overwrites-shadows
NULL

#' Merge a range of cells
#'
#' `merge_cells()` merges a rectangle of cells into a single displayed cell,
#' by setting [colspan()] and [rowspan()].
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#'
#' @details
#' `merge_cells(ht, c(min_row, max_row), c(min_col, max_col))` is equivalent to
#' ```
#'   colspan(ht)[min_row, min_col] <- max_col - min_col + 1
#'   rowspan(ht)[min_row, min_col] <- max_row - min_row + 1
#' ```
#'
#' @inheritSection span-overwrites-shadows Cell content
#'
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
merge_cells <- function(ht, row, col) {
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
#' `merge_across()` creates multicolumn cells within each row.
#' `merge_down()` creates multirow cells within each column.
#'
#' @inherit merge_cells params return
#'
#' @inheritSection span-overwrites-shadows Cell content
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
merge_across <- function(ht, row, col) {
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
merge_down <- function(ht, row, col) {
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
#' `merge_repeated_rows()` looks within each column for
#' contiguous groups of identical cells. These are merged
#' by setting [rowspan()]. Doing this helps remove redundant
#' information from the table.
#'
#' @inherit merge_cells params return
#'
#' @export
#'
#' @details
#' If `row` contains gaps, results may be unexpected (and a warning is given).
#'
#' @inheritSection span-overwrites-shadows Cell content
#'
#' @family cell merging
#'
#' @examples
#' ht <- as_hux(jams[c(1, 2, 2, 3, 3, 4), ])
#' ht <- add_columns(ht, c("Sugar", "30%", "40%", "30%", "40%", "30%"),
#'   after = 1
#' )
#' ht
#' merge_repeated_rows(ht)
#' merge_repeated_rows(ht, everywhere, "Type")
merge_repeated_rows <- function(ht, row, col) {
  assert_that(is_huxtable(ht))
  row <- get_rc_spec(ht, row, 1)
  col <- get_rc_spec(ht, col, 2)
  if (is.logical(row)) row <- which(row)
  if (is.logical(col)) col <- which(col)

  if (length(row) > 1L && !all(row == seq(min(row), max(row)))) {
    warning("Non-contiguous rows: ", paste(row, collapse = ", "))
  }
  for (cc in col) {
    contents <- ht[row, ][[cc]] # gets a vector
    contents <- paste0("", contents) # materializes NAs
    new <- c(TRUE, contents[-length(contents)] != contents[-1])
    new <- which(new)
    # spans gets the lengths between every new cell
    # we add length(contents) + 1 to spans, not to new, so that row[new]
    # can work in the next line:
    spans <- diff(c(new, length(contents) + 1))
    rowspan(ht)[row[new], cc] <- spans
  }

  ht
}
