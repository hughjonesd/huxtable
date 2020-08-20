
#' @import assertthat
NULL


#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j,name Columns to select.
#' @param drop Only included for compatibility with `[.data.frame`. Do not use.
#'
#' @return `[` returns a huxtable. `$` and `[[` return data from the
#'   underlying data frame.
#'
#' @export
#' @rdname extract-methods
#' @details
#'
#' # Replacing existing rows and columns
#'
#' For the replacement function `[<-`, if `value` is a huxtable, then its
#' properties will be copied into `x`. Replacement functions `$<-` and `[[<-`
#' replace existing data without affecting any properties.
#'
#' # Adding new rows and columns
#'
#' If new columns or rows are created, then properties will be copied
#' from the last column or row of `x`, or from `value` if `value` is a huxtable.
#'
#' These methods are stricter than their data frame equivalents in some places.
#' You can't add new rows or column at a numeric location without specifying all
#' intervening rows/columns. New values must have the appropriate dimensions
#' (vectors will be interpreted appropriately).
#'
#' @examples
#' jams[1:3, ]
#' class(jams[1:3, ])
#' jams[, 1]
#' jams$Type
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  if (! missing(drop) && drop) {
    stop("You can't use `drop = TRUE` to subset a huxtable. Use `[[` or convert explicitly.")
  }
  # effectively nargs() is the "number of commas in the call, plus one".
  # E.g. nargs() is 2 if a function is called like f(a=,b=).
  # whereas a and b will still be "missing".
  # So: [,x] -> missing_i, nargs() 2
  n_idx <- nargs() - ! missing(drop) - 1L
  if (n_idx == 1L) { # called like ht[x]
    if (missing(i)) {
      return(x)
    } else {
      j <- i
      i <- seq_len(nrow(x))
    }
  }
  ix <- normalize_index(i, nrow(x), rownames(x))
  jx <- normalize_index(j, ncol(x), colnames(x))

  if (any(is.na(ix))) stop("Bad row subscripts: ",
        paste(i[is.na(ix)], collapse = ","))
  if (any(is.na(jx))) stop("Bad column subscripts: ",
        paste(j[is.na(jx)], collapse = ","))
  res <- subset_rows(x, ix)
  res <- subset_cols(res, jx)

  return(res)
}


#' @param value A matrix, data frame, huxtable or similar object.
#'
#' @rdname extract-methods
#' @export
#'
#' @examples
#' prices <- huxtable(c("Price", 1.70, 2.00, 2.20))
#' number_format(prices) <- 2
#' bold(prices) <- TRUE
#' jams[, 2] <- prices
#' jams
#'
#' data(jams)
#' jams$price <- c("Price", 1.70, 2.00, 2.20)
#' jams
`[<-.huxtable` <- function (x, i, j, value) {
  # for ht[] <- x, nargs() is 3
  # for ht[1] <- x, nargs() is still 3
  # for ht[,1] <- x, nargs() is 4
  # for ht[1,1] <- x, nargs() is still 4
  # for ht[1,] <- x, nargs() is still 4

  n_idx <- nargs() - 2L
  if (n_idx == 1) {
    if (missing(i)) i <- seq_len(ncol(x))
    j <- i
    i <- seq_len(nrow(x))
  }

  ix <- normalize_index(i, nrow(x), rownames(x))
  jx <- normalize_index(j, ncol(x), colnames(x))

  if (is.null(value)) {
    if (! identical(ix, seq_len(nrow(x)))) stop("You can only set entire columns to NULL")
    return(delete_cols(x, jx))
  }

  # if any i and j are greater than nrow/ncol x, then cbind/rbind `x` with
  # the corresponding rows of `value`; remove these values of i/j; remove
  # the associated rows/cols of value; and proceed
  new_rows <- is.na(ix)
  new_cols <- is.na(jx)
  if (any(new_rows) && any(new_cols)) {
    stop("You can't simultaneously assign new rows and columns")
  }
  if (any(new_rows)) {
    # drop = FALSE is necessary since value may be a non-huxtable
    if (is_vectorish(value)) {
      assert_that(length(new_rows) == 1)
      new_value <- value
    } else {
      new_value <- value[new_rows, , drop = FALSE]
    }
    rn <- rownames(x)
    x <- rbind(x, new_value, copy_cell_props = TRUE)
    if (is.character(i)) rownames(x) <- c(rn, i[new_rows])
    ix <- ix[! new_rows]
  }

  if (any(new_cols)) {
    if (is_vectorish(value)) {
      assert_that(length(new_cols) == 1)
      new_value <- value
    } else {
      new_value <- value[, new_cols, drop = FALSE]
    }
    cn <- colnames(x)
    x <- cbind(x, new_value, copy_cell_props = TRUE)
    if (is.character(j)) colnames(x) <- c(cn, j[new_cols])
    jx <- jx[! new_cols]
  }

  # what to do about spans? One possibility: break `value` up into
  # huxtables that will be contiguous, using `[.huxtable`. Then we have
  # dealt appropriately with spans, somehow.

  # the below changes the data appropriately and retains all attributes, including
  # class. As we have already dealt with new rows/cols, we still have a valid
  # huxtable - all attributes have the correct dimensions:
  res <- NextMethod()
  if (is_hux(value)) {
    res <- replace_properties(res, ix, jx, value)
  }

  return(res)
}


#' @rdname extract-methods
#' @export
`$<-.huxtable` <- function (x, name, value) {
  assert_that(is.string(name))
  idx <- match(name, names(x))

  if (is.null(value)) {
    return(delete_cols(x, idx))
  } else if (is.na(idx)) {
    res <- cbind(x, value, copy_cell_props = TRUE)
    colnames(res)[ncol(x) + 1] <- name
    return(res)
  } else {
    return(NextMethod())
  }
}


#' @rdname extract-methods
#' @export
`[[<-.huxtable` <- function (x, i, j, value) {
  n_idx <- nargs() - 2
  if (n_idx == 1) {
    assert_that(is.scalar(i))
    jx <- normalize_index(i, ncol(x), colnames(x))
  } else {
    assert_that(is.scalar(i))
    assert_that(is.scalar(j))
    ix <- normalize_index(i, nrow(x), rownames(x))
    jx <- normalize_index(j, ncol(x), colnames(x))
  }

  if (is.null(value)) {
    if (n_idx != 1) stop("Can't set `ht[[row, column]] <- NULL`. ",
          "To delete a column set `ht[[column]] <- NULL`.")
    return(delete_cols(x, jx))
  }

  # data frames do weird things. We only add new columns via [[.
  if (is.na(jx)) {
    if (n_idx != 1) stop(
          "Can't add columns with `ht[[row, column]] <- new_col`. ",
          "Use `ht[[column]] <- new_col`.")
    res <- cbind(x, value, copy_cell_props = TRUE)
    if (is.character(i)) colnames(res) <- c(colnames(x), i)
    return(res)
  } else {
    return(NextMethod())
  }
}

