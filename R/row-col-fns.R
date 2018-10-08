
#' @import assertthat
NULL


#' Return every n row or column numbers
#'
#' This is a convenience function to use in row or column specifications.
#' In this context,
#' `every(n, from)` will return `from, from + n, ...,` up to the number of rows
#' or columns of the huxtable. `evens` and `odds` return even and odd
#' numbers, i.e. they are equivalent to `every(2, 2)` and `every(2, 1)` respectively.
#' `everywhere` returns all rows or columns, equivalently to `every(1)`.
#'
#' @param n A number (at least 1)
#' @param from A number (at least 1)
#' @param ht An object with a `dim` attribute like a matrix or data frame.
#' @param dimension Number of the dimension to use.
#'
#' @details
#' Technically, `every` returns a 2-argument function which can be called like
#' `f(ht, dimension)`. See [rowspecs] for details.
#'
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:10, b = 1:10)
#' set_background_color(ht,
#'       evens, everywhere,
#'       "grey95")
#' set_background_color(ht,
#'       every(3), everywhere,
#'       "grey95")
#'
every <- function(n = 1, from = n) {
  assert_that(is.count(n), is.count(from))

  return(
    function(ht, dimension) {
      ndim <- dim(ht)[dimension]
      if (ndim < from) return(numeric(0))
      seq(from, ndim, n)
    }
  )
}

#' @rdname every
#' @export
everywhere <- every(1, 1)

#' @rdname every
#' @export
evens <- every(2, 2)

#' @rdname every
#' @export
odds  <- every(2, 1)

#' Return the last n rows or columns
#'
#' This is a convenience function to use in row and column specifications. In that context, it
#' returns the last n row or column numbers of the huxtable.
#'
#' @param n Number of rows to return.
#'
#' @details
#' Technically, `final` returns a two-argument function - see [rowspecs] for more details.
#'
#' @export
#'
#' @examples
#' data(jams)
#'
#' set_bold(jams, final(2), final(1), TRUE)
final <- function(n = 1) {
  assert_that(is.count(n))

  return(
    function(ht, dimension) {
      ndim <- dim(ht)[dimension]
      if (ndim == 0) return(integer(0))
      low <- max(1, ndim + 1 - n)
      sort(ndim:low)
    }
  )
}

#' Different ways to select rows and columns
#'
#' This help page describes how to use the `row` and `col` arguments in `set_*` functions.
#' @section The basics:
#'
#' The `set_*` functions for cell properties all have arguments like this:
#' `set_property(ht, row, col, value, byrow = FALSE)`.
#'
#' You can treat `row` and `col` arguments like arguments for
#' \link[=[.data.frame]{data frame subsetting}. For example, you can use `row = 1:3` to get the
#' first three rows, `col = "salary"` to specify the column named "salary", or `row = ht$salary >=
#' 50000` to specify rows where a condition is true.
#'
#' There are also a few extra tricks you can use:
#'
#' * Write `set_property(ht, x)`, omitting `row` and `col`, to set the property to `x` for all cells.
#' * Use `everywhere` to refer to all rows or all columns.
#' * Use `final(n)` to refer to the last n rows or columns.
#' * Use `evens` to get only even rows/columns and `odds` for only odd ones.
#' * Use \code{\link[=every]{every(n, from = m)}} to get every nth row/column starting at row/column m.
#' * Use `dplyr` functions like `starts_with`, `contains` and `matches` to
#'    specify columns (but not rows). See \code{\link[tidyselect]{select_helpers}} for a full list.
#' * Set `byrow = TRUE` to set properties by row rather than by column.
#'
#' @section The gory details:
#'
#' How the row and col arguments are parsed depends on the number of arguments passed to the `set_*`
#' function.
#'
#' * If there are two arguments (excluding `byrow`) then the second argument is taken as the
#'     value and is set for all rows and columns.
#' * If there are four arguments:
#'     * If `row` or `col` is numeric, character or logical, it is evaluated just as in standard
#'         subsetting. `col` will be evaluated in a special context provided by [tidyselect::with_vars()]
#'         to allow the use of dplyr functions.
#'     * If `row` or `col` is a function,it is called with two arguments: the huxtable,
#'        and the dimension number being evaluated, i.e. 1 for rows, 2 for columns. It must return a vector
#'        of column indices. [evens()], [odds()], [every()] and [final()]
#'        return functions for this purpose.
#'
#' @name rowspecs
#'
#' @examples
#' data(jams)
#'
#' set_bold(jams, 2:4, 1:2, TRUE)
#' set_background_color(jams, evens, everywhere,
#'       "grey95")
#' set_bold(jams, everywhere,
#'       tidyselect::matches("yp"), TRUE)
#'
#' set_text_color(jams, 2:4, 1:2,
#'       c("red", "violetred", "purple"))
NULL

#' @name where
#' @rdname huxtable-deprecated
#' @export
NULL
# documenting the NULL object stops roxygen trying to print a usage section
# which causes R CMD check to throw a wobbly
where <- function(expr) {
  .Deprecated(package = 'huxtable')
  which(expr, arr.ind = TRUE)
}


#' @name is_a_number
#' @rdname huxtable-deprecated
#' @details To replace `is_a_number` use e.g. `! is.na(as.numeric(x))`
#' @export
NULL
is_a_number <- function(x) {
  if (is.data.frame(x)) {
    if (nrow(x) == 0) return(matrix(FALSE, 0, ncol(x)))
    res <- sapply(x, is_a_number)
    dim(res) <- dim(x)
    return(res)
  } else return(! is.na(suppressWarnings(as.numeric(x))))
}


get_rc_spec <- function (ht, obj, dimno) {
  dim_length <- dim(ht)[dimno]
  if (missing(obj)) return(seq_len(dim_length))

  # You can't evaluate obj before running the tidyselect; otherwise functions like starts_with throw an error.
  result <- if (dimno == 2) {
    tidyselect::with_vars(colnames(ht), if (is.function(obj)) obj(ht, dimno) else obj)
  } else {
    if (is.function(obj)) obj(ht, dimno) else obj
  }

  result
}
