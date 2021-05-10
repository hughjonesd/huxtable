
#' @importFrom stats setNames
#' @import assertthat
NULL


#' Insert one huxtable into another
#'
#' These functions combine two huxtables or similar objects and
#' return the result.
#'
#' @param x,y Huxtables or objects that can be converted by as_hux
#' @param after Row or column after which `y` is inserted. Can be 0.
#'   Can be a row or column name. The default adds `y` to the
#'   end of `x`.
#' @param copy_cell_props Logical. Passed to [rbind.huxtable()] or
#'   [cbind.huxtable()].
#'
#' @return A huxtable.
#' @export
#'
#' @details
#' Arguments in `...` can include `copy_cell_props`.
#'
#' @seealso [insert_row()] and [insert_column()], which insert
#' multiple values into a single row.
#'
#' @examples
#'
#' ht <- hux("Gooseberry", 2.15)
#' add_rows(jams, ht)
#' add_rows(jams, ht, after = 1)
#'
#' mx <- matrix(
#'       c("Sugar", "50%", "60%", "40%",
#'       "Weight (g)", 300, 250, 300),
#'       4, 2)
#' add_columns(jams, mx)
add_rows <- function (x, y, after = nrow(x), copy_cell_props = TRUE) {
  add_row_cols(x, y, after, dimno = 1,
        copy_cell_props = copy_cell_props)
}


#' @export
#' @rdname add_rows
add_columns <- function (x, y, after = ncol(x),
      copy_cell_props = TRUE) {
  add_row_cols(x, y, after, dimno = 2,
        copy_cell_props = copy_cell_props)
}


#' Insert a row or column
#'
#' These convenience functions wrap `cbind` or `rbind` for huxtables, to insert
#' a single row or column.
#'
#' @param ht A huxtable.
#' @param ... Cell contents.
#' @param after Insert the row/column after this position. 0 (the default) inserts as the first row/column.
#' @param fill String. If `...` contains fewer elements than there are columns/rows to
#' fill, the remaining cells will be filled with this.
#' @param rowspan,colspan Scalar integer. Sets the rowspan or colspan of the *first* cell only.
#' The default `NULL` throws an error if there are too few elements.
#' @param copy_cell_props Copy cell properties from the previous row or column (if after > 0). See [cbind.huxtable()].
#' @details
#' In `insert_column` only, you can use a column name for `after`.
#'
#' Even if `colspan` or `rowspan` are greater than 1, you must still provide
#' values for the hidden cells. Use `fill = ""` for this.
#'
#' @return The modified huxtable
#' @seealso [add_rows()] and [add_columns()], which insert multiple rows/columns at once.
#' @export
#'
#' @examples
#' insert_row(jams,
#'         c("Gooseberry", 2.15),
#'         after = 1
#'       )
#'
#' insert_column(jams,
#'         c("Sugar", "50%", "60%", "40%"),
#'         after = "Price"
#'       )
#'
#' insert_column(jams,
#'         "Sugar",
#'         after = "Price",
#'         fill = "50%"
#'       )
#'
#' # don't forget to use `fill`:
#' insert_row(jams,
#'         "Jams and prices",
#'         fill = "",
#'         colspan = 2
#'       )
insert_column <- function (ht, ..., after = 0, fill = NULL, rowspan = 1,
      copy_cell_props = TRUE) {
  # is.count would complain about 0
  assert_that(
          is.scalar(after),
          is.number(after) || is.string(after),
          is.null(fill) || is.scalar(fill),
          is.count(rowspan)
        )
  if (is.number(after)) assert_that(after >= 0, after <= ncol(ht))
  if (is.string(after)) {
    assert_that(has_name(ht, after))
    after <- match(after, colnames(ht))
  }

  ht1 <- ht[, seq_len(after)]
  ht2 <- NULL
  if (after < ncol(ht)) {
    ht2 <- ht[, seq(after + 1, ncol(ht), 1)]
  }

  to_insert <- c(...)
  if (length(to_insert) < nrow(ht)) {
    to_insert <- c(to_insert, rep(fill, nrow(ht) - length(to_insert)))
  }
  res <- if (! is.null(ht1)) cbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) cbind(res, ht2) else res

  rowspan(res)[1, after + 1] <- rowspan
  rownames(res) <- rownames(ht)

  res
}


#' @rdname insert_column
#'
#' @export
insert_row <- function (ht, ..., after = 0, fill = NULL, colspan = 1, copy_cell_props = TRUE) {
  assert_that(
          is.scalar(after),
          is.number(after),
          after >= 0,
          after <= nrow(ht),
          is.null(fill) || is.scalar(fill),
          is.count(colspan)
        )




  ht1 <- ht[seq_len(after), ]
  ht2 <- NULL
  if (after < nrow(ht)) {
    ht2 <- ht[seq(after + 1, nrow(ht), 1), ]
  }

  to_insert <- c(...)
  if (length(to_insert) < ncol(ht)) {
    to_insert <- c(to_insert, rep(fill, ncol(ht) - length(to_insert)))
  }
  res <- if (! is.null(ht1)) rbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) rbind(res, ht2) else res

  colspan(res)[after + 1, 1] <- colspan
  colnames(res) <- colnames(ht)

  res
}



#' Combine rows or columns
#'
#' These methods are called when one argument to `cbind`/`rbind` is a
#' huxtable. As well as combining cell contents, they copy table, row,
#' column and/or cell properties into the returned result.
#'
#' @param ... Vectors, matrices, or huxtables.
#' @param deparse.level Unused.
#' @param copy_cell_props Cell properties to copy from neighbours (see below).
#'
#' @return A huxtable.
#'
#' @details
#' Table properties will be taken from the first argument which is a huxtable. So will
#' row properties (for cbind) and column properties (for rbind).
#'
#' If some of the inputs are not huxtables, and `copy_cell_props` is`TRUE`,
#' then cell properties will be copied to non-huxtables. Objects on the left
#' or above get priority over those on the right or below.
#'
#' If `copy_cell_props` is `FALSE`, cells from non-huxtable objects will get the default properties.
#'
#' You cannot bind huxtables with data frames, since the R method dispatch will always
#' call the data frame method instead of the huxtable-specific code. For a solution, see
#' [add_columns()].
#'
#'
#' @examples
#'
#' sugar <- c("Sugar", "40%", "35%", "50%")
#' jams <- set_bold(jams, 1, everywhere)
#' cbind(jams, sugar)
#' cbind(jams, sugar,
#'      copy_cell_props = FALSE)
#'
#' jams <- set_text_color(jams,
#'      everywhere, 1, "red")
#' rbind(jams, c("Damson", 2.30))
#' rbind(jams, c("Damson", 2.30),
#'      copy_cell_props = FALSE)
#'
#' @export
cbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  assert_that(is.flag(copy_cell_props))
  f <- function (obj1, obj2) bind_cols_2(obj1, obj2,
        copy_cell_props = copy_cell_props)
  res <- Reduce(f, list(...))
  colnames(res) <- dot_or_dim_names(..., dimension = 2)

  res
}


#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  assert_that(is.flag(copy_cell_props))
  f <- function (obj1, obj2) bind_rows_2(obj1, obj2,
        copy_cell_props = copy_cell_props)
  res <- Reduce(f, list(...))
  rownames(res) <- dot_or_dim_names(..., dimension = 1)

  res
}


#' Transpose a huxtable
#'
#' `t()` switches a huxtable so rows become columns and columns become rows.
#'
#' @param x A huxtable.
#'
#' @return The transposed huxtable.
#'
#' @details
#' Row and column spans of `x` will be swapped, as will column widths and row heights,
#' table width and height, and cell borders (bottom becomes right, etc.).
#' Other properties - in particular, alignment, vertical alignment and rotation - will be
#' preserved.
#' @examples
#' ht <- huxtable(
#'         a = 1:3,
#'         b = letters[1:3],
#'         autoformat = FALSE
#'       )
#' bottom_border(ht)[3,] <- 1
#' ht
#' t(ht)
#'
#' @export
t.huxtable <- function (x) {
  res <- as_hux(NextMethod(), add_colnames = FALSE, autoformat = FALSE)
  for (a in setdiff(huxtable_cell_attrs, c("colspan", "rowspan", "height", "width"))) {
    attr(res, a) <- t(attr(x, a))
  }
  attr(res, "colspan") <- t(attr(x, "rowspan"))
  attr(res, "rowspan") <- t(attr(x, "colspan"))
  attr(res, "width")   <- attr(x, "height")
  attr(res, "height")  <- attr(x, "width")

  bottom_border(res) <- t(right_border(x))
  right_border(res)  <- t(bottom_border(x))
  left_border(res)   <- t(top_border(x))
  top_border(res)    <- t(left_border(x))
  bottom_border_style(res) <- t(right_border_style(x))
  right_border_style(res)  <- t(bottom_border_style(x))
  left_border_style(res)   <- t(top_border_style(x))
  top_border_style(res)    <- t(left_border_style(x))
  bottom_border_color(res) <- t(right_border_color(x))
  right_border_color(res)  <- t(bottom_border_color(x))
  left_border_color(res)   <- t(top_border_color(x))
  top_border_color(res)    <- t(left_border_color(x))

  row_height(res) <- col_width(x)
  col_width(res)  <- row_height(x)
  rownames(res)   <- colnames(x)
  colnames(res)   <- rownames(x)
  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(x, a)
  }

  res
}
