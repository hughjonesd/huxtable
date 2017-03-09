


#' Return Every n Row or Column Numbers
#'
#' This is a convenience function to use in row or column specifications.
#' In this context,
#' \code{every(n, from)} will return \code{from, from + n, ...,} up to the number of rows
#' or columns of the huxtable. \code{evens(from)} and \code{odds(from)} return even and odd
#' numbers starting from \code{from}.
#'
#' @param n A number (at least 1)
#' @param from A number (at least 1)
#'
#' @details
#' Technically, \code{every} returns a 2-argument function which can be called as
#' \code{f(hux, dimension)}. See \code{\link{row-col-specs}} for details.
#'
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:10, b = 1:10)
#' ht <- set_background_color(ht, every(3), 1:2, 'wheat')
#' ht <- set_align(ht, evens(), 1:2, 'right')
#' ht <- set_align(ht, odds(5), 1:2, 'center')
#' align(ht)
#' background_color(ht)
#'
every <- function(n, from = n) {
  stopifnot(is.numeric(n))
  stopifnot(n >= 1)
  stopifnot(is.numeric(from))
  stopifnot(from >= 1)
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
#' @examples
evens <- function(from = 2) every(2, ceiling(from/2) * 2)

#' @rdname every
#' @export
odds  <- function(from = 1) every(2, ceiling((from - 1)/2) * 2 + 1)

#' Return the Last n Rows or Columns
#'
#' This is a convenience function to use in row and column specifications. In that context, it
#' returns the last n row or column numbers of the huxtable.
#' @param n
#'
#' @return
#'
#' @details
#'
#' Technically, \code{last} returns a two-argument function - see \code{\link{row-col-specs}} for more details.
#'
#' @export
#'
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
#' ht <- set_align(ht, last(2), last(1), 'right')
#' align(ht)
#'
#' last(3)(ht, 1) # last 3 rows
#' last(3)(ht, 2) # last 3 columns
last <- function(n = 1) {
  stopifnot(is.numeric(n))
  stopifnot(n >= 1)
  return(
    function(ht, dimension) {
      ndim <- dim(ht)[dimension]
      if (ndim == 0) return(integer(0))
      low <- max(1, ndim + 1 - n)
      sort(ndim:low)
    }
  )
}

#' Row and Column Specifications
#'
#' The \code{set_*} functions in huxtable all share arguments like
#' \code{set_property(ht, row, col, value)}.
#'
#' You can treat \code{row} and \code{col} arguments just like arguments to \code{[} for a data frame.
#' But there are a few extra tricks:
#'
#'\itemize{
#'  \item In \code{col} you can use column names in ranges, so instead of \code{set_property(ht, , 2:3)}
#'    you could write \code{set_property(ht, , col2:col3)}.
#'  \item Use \code{\link[=last]{last(n)}} to refer to the last n rows or columns.
#'  \item Use \code{\link[=evens]{evens()}} to get only even rows/columns and \code{\link[=odds]{odds()}}
#'    for only odd ones.
#'  \item Use \code{\link{every(n, from = m)}} to get every nth row/column starting at row/column m.
#'  \item Use \code{\link[=where]{where(cond)}}, and omit the \code{col} argument, to get cells where \code{cond} is
#'    \code{TRUE}.
#'}
#'
#'
#' @section The gory details
#'
#' Here is how the row and col arguments are parsed:
#'
#' \itemize{
#'   \item Names in \code{col} get replaced by column indices, as in \code{\link[base]{subset}}.
#'   \item Expressions in \code{row} get evaluated in the context of the huxtable object,
#'     as in \code{\link[base]{subset}}.
#'   \item If \code{row} or \code{col} is numeric, character or logical, it is evaluated just as in standard
#'     subsetting - see \code{\link[base]{Extract.data.frame}}.
#'   \item If \code{row} or \code{col} is a function, it gets called to give a set of column indices - see below.
#'   \item If \code{row} or \code{col} is missing, all rows or columns are returned, as in standard subsetting.
#' }
#'
#' If \code{row} or \code{col} is a function, it is called with two arguments: the huxtable, and the dimension
#' number being evaluated, i.e. 1 for rows, 2 for columns. It must return a vector of column indices.
#' \code{\link{evens}}, \code{\link{odds}}, \code{\link{every}} and \code{\link{last}} return
#' functions for this purpose.
#' @name row-col-specs
#'
#'
NULL

#' Return Array Indices Where Expression Is True
#'
#' This is a simple wrapper around \code{which(..., arr.ind = TRUE)}, for
#' use in \code{\link{row-col-spec}} expressions.
#' @param expr An R expression
#'
#' @export
#'
#' @return A matrix of row and column indices of cells where \code{expr} is \code{TRUE}.
#'
#' @examples
#' ht <- hux(a = 1:3, b = 4:6, add_colnames = TRUE)
#' where(ht)
where <- function(expr) {which(expr, arr.ind = TRUE)}

#' Does an Object Look Like a Number?
#'
#' A convenience function that returns \code{TRUE} if an object either is numeric or
#' can be converted to a number. For data frames, it returns a matrix of the same
#' dimensions as the data frame.
#' @param x An object.
#'
#' @return A logical object with the same dimensions as \code{x}.
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
  ndim <- dim(ht)[dimno]
  if (missing(obj)) return(seq_len(ndim))
  nl <- as.list(seq_len(ncol(ht)))
  names(nl) <- colnames(ht)
  obj <- eval(lazyeval::expr_find(obj), nl, parent.frame())
  if (is.function(obj)) return(obj(ht, dimno)) else return(obj)
}
