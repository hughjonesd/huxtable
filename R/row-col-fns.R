


#' Return every n row or column numbers
#'
#' This is a convenience function to use in row or column specifications.
#' In this context,
#' \code{every(n, from)} will return \code{from, from + n, ...,} up to the number of rows
#' or columns of the huxtable. \code{evens(from)} and \code{odds(from)} return even and odd
#' numbers starting from \code{from}. Called with no arguments, \code{every()} returns
#' all rows or columns.
#'
#' @param n A number (at least 1)
#' @param from A number (at least 1)
#'
#' @details
#' Technically, \code{every} returns a 2-argument function which can be called as
#' \code{f(hux, dimension)}. See \code{\link{rowspecs}} for details.
#'
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:10, b = 1:10)
#' ht <- set_background_color(ht, every(3), every(), 'wheat')
#' ht <- set_align(ht, evens(), 1:2, 'right')
#' ht <- set_align(ht, odds(5), 1:2, 'center')
#' align(ht)
#' background_color(ht)
#'
every <- function(n = 1, from = n) {
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
evens <- function(from = 2) every(2, ceiling(from/2) * 2)

#' @rdname every
#' @export
odds  <- function(from = 1) every(2, ceiling((from - 1)/2) * 2 + 1)

#' Return the last n rows or columns
#'
#' This is a convenience function to use in row and column specifications. In that context, it
#' returns the last n row or column numbers of the huxtable.
#'
#' @param n Number of rows to return.
#'
#' @details
#' Technically, \code{final} returns a two-argument function - see \code{\link{rowspecs}} for more details.
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

#' Row and column specifications
#'
#' @section The basics:
#'
#' The \code{set_*} functions for cell properties all have arguments like this:
#' \code{set_property(ht, row, col, value, byrow = FALSE)}.
#'
#' You can treat \code{row} and \code{col} arguments like arguments to \code{[} for a data frame.
#' But there are a few extra tricks:
#'
#'\itemize{
#'  \item Write \code{set_property(ht, x)} to set the property to \code{x} for all cells.
#'  \item Use \code{\link[=final]{final(n)}} to refer to the last n rows or columns.
#'  \item Use \code{\link[=evens]{evens()}} to get only even rows/columns and \code{\link[=odds]{odds()}}
#'    for only odd ones.
#'  \item Use \code{\link[=every]{every(n, from = m)}} to get every nth row/column starting at row/column m.
#'    Use \code{every(1)} or just \code{every()} to get all rows or columns.
#'  \item Use \code{\link[=where]{where(cond)}}, and omit the \code{col} argument, to get cells where \code{cond} is
#'    \code{TRUE}.
#'  \item Omit both \code{row} and \code{col} arguments to set a property for all cells.
#'  \item Set \code{byrow = TRUE} to set properties by row rather than by column.
#'}
#'
#'
#' @section The gory details:
#'
#' Here is how the row and col arguments are parsed:
#'
#' \itemize{
#'   \item If there are two arguments (excluding \code{byrow}) then the second argument is taken as the
#'     value and is set for all rows and columns.
#'   \item If there are three arguments, then the third argument is taken as the value, and
#'     \code{row} must be a matrix with two columns. Each row of this matrix
#'     gives the row, column indices of a single cell. This uses R's little known feature of
#'     subsetting with matrices - see \code{\link[base]{Extract}}.
#'   \item If there are four arguments:
#'     \itemize{
#'       \item If \code{row} or \code{col} is numeric, character or logical, it is evaluated just as in standard
#'         subsetting.
#'       \item If \code{row} or \code{col} is a function,it is called with two arguments: the huxtable,
#'         and the dimension number being evaluated, i.e. 1 for rows, 2 for columns. It must return a vector
#'         of column indices. \code{\link{evens}}, \code{\link{odds}}, \code{\link{every}} and \code{\link{final}}
#'        return functions for this purpose.
#'     }
#' }
#'
#'
#' @name rowspecs
#'
#' @examples
#' ht <- huxtable(a = 1:5, b = 5:1)
#' ht <- set_font(ht, a >= 2 & b >= 2, a:b, 'times')
#' font(ht)
#' ht <- set_font(ht, where(ht == 1), 'palatino')
#' font(ht)
#' ht <- set_font(ht, odds(), evens(), 'dingbats')
#' font(ht)
#' ht <- set_align(ht, every(3, from = 1), a:b, 'right')
#' align(ht)
NULL

#' Return array indices where expression is true
#'
#' This is a simple wrapper around \code{which(..., arr.ind = TRUE)}, for
#' use in \code{\link[=rowspecs]{row specifications}}.
#' @param expr An R expression
#'
#' @export
#'
#' @return A matrix of row and column indices of cells where \code{expr} is \code{TRUE}.
#'
#' @examples
#' ht <- hux(a = 1:3, b = 4:6, add_colnames = TRUE)
#' where(ht > 2)
#' where(is_a_number(ht))
#'
where <- function(expr) {which(expr, arr.ind = TRUE)}

#' Does an object look like a number?
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

  # if (dimno == 1) {
  #   obj <- eval(lazyeval::expr_find(obj), ht, parent.frame())
  #   if (is.logical(obj)) obj <- obj & ! is.na(obj)
  # } else {
  #   nl <- as.list(seq_len(ncol(ht)))
  #   names(nl) <- colnames(ht)
  #   obj <- eval(lazyeval::expr_find(obj), nl, parent.frame())
  # }

  if (is.function(obj)) return(obj(ht, dimno)) else return(obj)
}
