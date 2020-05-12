
#' @import assertthat
NULL


#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j,name Columns to select.
#' @param drop Only included for compatibility with `[.data.frame`. Do not use.
#'
#' @return A huxtable.
#' @export
#' @rdname extract-methods
#' @details
#' `[` always returns a huxtable, while `$` and `[[`
#' return the underlying data.
#'
#' For the replacement function `[<-`, if `value` is a huxtable, then its cell properties will be
#' copied into `x`. In addition, if `value` fills up an entire column, then column properties
#' will be copied into the replaced columns of `x`, and if it fills up an entire row, then
#' row properties will be copied into the replaced rows of `x`.
#'
#' Replacement functions `$<-` and `[[<-` replace existing data without affecting any properties.
#' @examples
#' jams[1:3, ]
#' class(jams[1:3, ])
#' jams[, 1]
#' jams$Type
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  n_idx <- nargs() - !missing(drop) - 1L
  missing_i <- missing(i)
  missing_j <- missing(j) # evaluate before swapping j and i
  if (missing_i && missing_j) return(x)

  if (! missing(drop) && drop) {
    stop("Can't use `drop = TRUE` to subset a huxtable. Use `[[` instead.")
  }
  ss <- if (n_idx <= 1) {
    # avoids a warning from `[.data.frame`
    NextMethod("[", as.data.frame(x))
  } else {
    NextMethod("[", as.data.frame(x), drop = FALSE)
  }

  if (n_idx == 1L) {
    j <- i
    i <- seq_len(nrow(x))
  }

  if (! missing_i && is.character(i)) i <- which(rownames(x) %in% i)
  if (! missing_j && is.character(j)) j <- which(colnames(x) %in% j)
  for (a in huxtable_cell_attrs) {
    attr(ss, a) <- attr(x, a)[i, j, drop = drop]
  }
  for (a in huxtable_col_attrs) {
    attr(ss, a) <- attr(x, a)[j]
  }
  for (a in huxtable_row_attrs) {
    attr(ss, a) <- attr(x, a)[i]
  }
  for (a in huxtable_table_attrs) {
    attr(ss, a) <- attr(x, a)
  }

  class(ss) <- class(x)

  # we don't use `colspan<-` because this immediately triggers a `check_span_shadows`
  # which will error if the `rowspan` is not yet correct
  attr(ss, "colspan") <- pmin(colspan(ss), 1 + ncol(ss) - col(ss))
  attr(ss, "rowspan") <- pmin(rowspan(ss), 1 + nrow(ss) - row(ss))

  ss <- set_attr_dimnames(ss)
  ss
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
  res <- as.data.frame(NextMethod())

  if (ncol(res) < ncol(x)) {
    assert_that(is.null(value))
    # could be ht[,'foo'] <- NULL or ht['foo'] <- NULL so this is safest:
    idx <- which(! colnames(x) %in% colnames(res))
    res <- delete_props(res, idx, type = "cols")
  }
  if (ncol(res) > ncol(x)) {
    # Assumption: extra columns are on the right. Note that we may ALSO have more rows
    res <- merge_props(res, x, res[seq_len(nrow(x)), seq(ncol(x) + 1, ncol(res))], type = "cbind")
  }
  if (nrow(res) > nrow(x)) {
    # we may have already given res the appropriate attributes above; if so use them since x has too few cols
    first <- if (ncol(res) > ncol(x)) res[seq_len(nrow(x)), ] else x
    res <- merge_props(res, first, res[seq(nrow(x) + 1, nrow(res)), ], type = "rbind")
  }

  if (is_huxtable(value)) {
    if (! missing(i)) i <- if (is.character(i)) which(rownames(res) %in% i) else if (is.logical(i)) which(i) else i
    if (! missing(j)) j <- if (is.character(j)) which(colnames(res) %in% j) else if (is.logical(j)) which(j) else j
    for (a in huxtable_cell_attrs) {
      val <- attr(value, a)
      val <- matrix(val,
        nrow(res[i, j, drop = FALSE]),
        ncol(res[i, j, drop = FALSE]),
        byrow = (nrow(val) == 1)
      )

      attr(res, a)[i, j] <- val
    }
    if (missing(i) || identical(i, seq_len(nrow(res)))) {
      for (a in huxtable_col_attrs) attr(res, a)[j] <- attr(value, a)
    }
    if (missing(j) || identical(j, seq_len(ncol(res)))) {
      for (a in huxtable_row_attrs) attr(res, a)[i] <- attr(value, a)
    }
  }

  res <- set_attr_dimnames(res)
  class(res) <- class(x)
  return(res)
}


#' @rdname extract-methods
#' @export
`$<-.huxtable` <- function (x, name, value) {
  res <- as.data.frame(NextMethod())

  if (ncol(res) < ncol(x)) {
    stopifnot(is.null(value))
    idx <- if (is.character(name)) match(name, colnames(x)) else name
    res <- delete_props(res, idx, type = "cols")
  }
  if (ncol(res) > ncol(x)) {
    res <- merge_props(res, x, res[, seq(ncol(x) + 1, ncol(res))], type = "cbind")
  }

  res <- set_attr_dimnames(res)
  class(res) <- class(x)
  res
}


#' @rdname extract-methods
#' @export
`[[<-.huxtable` <- function (x, i, j, value) {
  res <- as.data.frame(NextMethod())
  # [[<- can be called with one or two indexes. The 2 index form doesn't extend columns, but does extend rows.
  # It can't delete either rows or columns though.
  # The one-index form only extends columns. It can also delete columns.
  if (ncol(res) < ncol(x)) {
    stopifnot(is.null(value))
    idx <- if (is.character(i)) match(i, colnames(x)) else i
    res <- delete_props(res, idx, type = "cols")
  }
  if (ncol(res) > ncol(x)) {
    # Assumption: extra columns are on the right
    res <- merge_props(res, x, res[, seq(ncol(x) + 1, ncol(res))], type = "cbind")
  }
  if (nrow(res) > nrow(x)) {
    res <- merge_props(res, x, res[seq(nrow(x) + 1, nrow(res)), ], type = "rbind")
  }

  res <- set_attr_dimnames(res)
  class(res) <- class(x)
  res
}
