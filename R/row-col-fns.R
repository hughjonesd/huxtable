
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
#' ht <- set_background_color(ht, every(3), everywhere, 'wheat')
#' background_color(ht)
#' ht <- set_align(ht, evens, 1:2, 'right')
#' ht <- set_align(ht, odds, 1:2, 'center')
#' align(ht)
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
#' ht <- hux(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
#' ht <- set_align(ht, final(2), final(1), 'right')
#' align(ht)
#'
#' final(3)(ht, 1) # last 3 rows
#' final(3)(ht, 2) # last 3 columns
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

#' Row and column specifications
#'
#' @section The basics:
#'
#' The `set_*` functions for cell properties all have arguments like this:
#' `set_property(ht, row, col, value, byrow = FALSE)`.
#'
#' You can treat `row` and `col` arguments like arguments to [data frame subsetting][base:Extract.data.frame].
#' But there are a few extra tricks:
#'
#' * Write `set_property(ht, x)`, omitting `row` and `col`, to set the property to `x` for all cells.
#' * Use [`everywhere`][everywhere] to refer to all rows or all columns.
#' * Use \code{\link[=final]{final(n)}} to refer to the last n rows or columns.
#' * Use [`evens`][evens] to get only even rows/columns and [`odds`][odds] for only odd ones.
#' * Use \code{\link[=every(n, from = m)]{every}} to get every nth row/column starting at row/column m.
#' * Use `dplyr` functions like `starts_with`, `contains` and `matches` to
#'    specify columns (but not rows). See [tidyselect::select_helpers()] for a full list.
#' * Use \code{\link[=where]{where(condition)}}, and omit the `col` argument, to get cells where `condition` is `TRUE`.
#' * Set `byrow = TRUE` to set properties by row rather than by column.
#'
#' @section The gory details:
#'
#' How the row and col arguments are parsed depends on the number of arguments passed to the `set_*`
#' function.
#'

#' * If there are two arguments (excluding `byrow`) then the second argument is taken as the
#'     value and is set for all rows and columns.
#' * If there are three arguments, then the third argument is taken as the value, and
#'     `row` must be a matrix with two columns. Each row of this matrix
#'     gives the row, column indices of a single cell. This uses R's little known feature of
#'     subsetting with matrices - see [base::Extract()].
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
#' ht <- huxtable(a = 1:5, b = 5:1)
#'
#' set_bold(ht, 2:4, 1:2, TRUE)
#' set_bold(ht, odds, evens, TRUE)
#' set_bold(ht, everywhere, tidyselect::matches('[aeiou]'), TRUE)
#'
#' set_bold(ht, where(ht == 1), TRUE)
#'
#' set_text_color(ht, 2:3, 1:2, c('red', 'blue'))
#' set_text_color(ht, 2:3, 1:2, c('red', 'blue'), byrow = TRUE)
NULL

#' Return array indices where expression is true
#'
#' This is a simple wrapper around `which(..., arr.ind = TRUE)`, for
#' use in [row specifications][rowspecs].
#' @param expr An R expression
#'
#' @export
#'
#' @return A matrix of row and column indices of cells where `expr` is `TRUE`.
#'
#' @examples
#' ht <- hux(a = 1:3, b = 4:6, add_colnames = TRUE)
#' where(ht > 2)
#' where(is_a_number(ht))
#'
where <- function(expr) which(expr, arr.ind = TRUE)

#' Does an object look like a number?
#'
#' A convenience function that returns `TRUE` if an object either is numeric or
#' can be converted to a number. For data frames, it returns a matrix of the same
#' dimensions as the data frame.
#' @param x An object.
#'
#' @return A logical object with the same dimensions as `x`.
#' @export
#'
#' @examples
#'
#' is_a_number(1.0)
#' is_a_number("1.0")
#' is_a_number("a")
#' ht <- hux(a = 1:3, b = 1:3, add_colnames = TRUE)
#' is_a_number(ht)
is_a_number <- function(x) {
  if (is.data.frame(x)) sapply(x, is_a_number) else ! is.na(suppressWarnings(as.numeric(x)))
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
