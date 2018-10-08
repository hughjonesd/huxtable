
# Subsetting and manipulation ----------------------------------------------------------------------

#' @importFrom stats setNames
#' @import assertthat
NULL


#' Insert one matrix into another.
#'
#' These functions combine two matrix-like objects and return the result.
#'
#' @param x A matrix-like object, e.g. a huxtable
#' @param y Matrix or vector to be inserted into `x`
#' @param after Row or column after which `y` is inserted. Can be 0. Can be a row or column name.
#'   By default, inserts `y` after the end of `x`.
#' @param ... Arguments passed to [rbind()] or [cbind()]
#'
#' @return For `add_rows`, the result of `rbind(x[1:after,], y, x[-(1:after),]`. For `add_columns`
#'   the same but with columns. `after = 0` and `after = nrow(x)` or `ncol(x)` are handled correctly.
#' @export
#'
#' @details
#' For `huxtable` objects, arguments in `...` can include `copy_cell_props`.
#'
#' @seealso [insert_row()] and [insert_column()], which insert multiple values into a single row.
#'
#' @examples
#' ht <- hux(
#'         Jam = c("Raspberry", "Plum"),
#'         Price = c(1.90, 1.80),
#'         add_colnames = TRUE
#'       )
#'
#' ht2 <- hux("Gooseberry", 2.10)
#' add_rows(ht, ht2)
#' add_rows(ht, ht2, after = 1)
#'
#' mx <- matrix(
#'       c("Sugar", "50%", "60%",
#'       "Weight (g)", 300, 250),
#'       3, 2)
#' add_columns(ht, mx, after = "Jam")
add_rows <- function (x, y, after = nrow(x), ...) {
  add_row_cols(x, y, after, dimno = 1, ...)
}


#' @export
#' @rdname add_rows
add_columns <- function (x, y, after = ncol(x), ...) {
  add_row_cols(x, y, after, dimno = 2, ...)
}


add_row_cols <- function (x, y, after, dimno, ...) {
  just_a_data_frame <- function (obj) inherits(obj, "data.frame", which = TRUE) == 1
  if (is_hux(x) && just_a_data_frame(y)) {
    y <- as_hux(y)
    attr(y, "from_real_hux") <- FALSE
  }
  if (is_hux(y) && just_a_data_frame(x)) {
    x <- as_hux(x)
    attr(x, "from_real_hux") <- FALSE
  }
  dims <- dim(x)
  end_idx <- dims[dimno]
  assert_that(is.numeric(dims))
  if (is.character(after)) {
    after <- match(after, dimnames(x)[[dimno]])
  }
  assert_that(is.number(after), after >= 0, after <= end_idx)

  second_idxes <- if (after < end_idx) seq(after + 1, end_idx) else integer(0)
  # for some reason `fn <- if (dimno==1) rbind else cbind` causes trouble...
  if (dimno == 1) {
    rbind(x[seq_len(after), ], y, x[second_idxes, ], ...)
  } else {
    cbind(x[, seq_len(after)], y, x[, second_idxes], ...)
  }
}


#' Insert a row or column
#'
#' These convenience functions wrap `cbind` or `rbind` for huxtables to insert
#' a single row.
#' @param ht A huxtable.
#' @param ... Cell contents.
#' @param after Insert the row/column after this position. 0 (the default) inserts as the first row/column.
#' @param copy_cell_props Copy cell properties from the previous row or column (if after > 0). See [cbind.huxtable()].
#' @details
#' In `insert_column` only, you can use a column name for `after`.
#' @return The modified huxtable
#' @seealso [add_rows()] and [add_columns()], which insert multiple rows/columns at once.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, c = 1:5)
#' insert_row(ht, 2.5, 2.5, 2.5,
#'       after = 2)
#'
#' insert_column(ht, 5:1)
#' insert_column(ht, 5:1, after = 3)
#' insert_column(ht, 5:1, after = "b")
insert_column <- function (ht, ..., after = 0, copy_cell_props = TRUE) {
  # is.count would complain about 0
  assert_that(is.scalar(after), is.number(after) || is.string(after))
  if (is.number(after)) assert_that(after >= 0, after <= ncol(ht))
  if (is.string(after)) {
    assert_that(has_name(ht, after))
    after <- match(after, colnames(ht))
  }

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[, seq(1, after, 1)]
  }
  if (after < ncol(ht)) {
    ht2 <- ht[, seq(after + 1, ncol(ht), 1)]
  }
  to_insert <- c(...)
  res <- if (! is.null(ht1)) cbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) cbind(res, ht2) else res

  res
}


#' @rdname insert_column
#'
#' @export
insert_row <- function (ht, ..., after = 0, copy_cell_props = TRUE) {
  # is.count would complain about 0
  assert_that(is.scalar(after), is.number(after), after >= 0, after <= nrow(ht))

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[seq(1, after, 1), ]
  }
  if (after < nrow(ht)) {
    ht2 <- ht[seq(after + 1, nrow(ht), 1), ]
  }
  to_insert <- c(...)
  res <- if (! is.null(ht1)) rbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) rbind(res, ht2) else res

  res
}


#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j,name Columns to select.
#' @param drop Not used.
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
#' Replacement functions `$<-` and `[[<-` change the data without affecting any properties.
#' @examples
#' ht <- huxtable(
#'         a = 1:3,
#'         b = letters[1:3]
#'       )
#'
#' ht[1:2, ]
#' ht[, 1]
#' ht$a
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- as.data.frame(x)[i, j, drop = drop]
  if (! missing(i) && is.character(i)) i <- which(rownames(x) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(x) %in% j)
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
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht2 <- huxtable(10:11, 12:13)
#' bold(ht2) <- TRUE
#' ht[2:3,] <- ht2
#' ht
#'
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
      val <- matrix(val, nrow(res[i, j, drop = FALSE]), ncol(res[i, j, drop = FALSE]), byrow = TRUE)
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

#' Combine rows or columns
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
#' If some of the inputs are not huxtables, and `copy_cell_props` is a character vector of cell
#' properties, then the named cell properties will be copied to non-huxtables. Objects on the left
#' or above get priority over those on the right or below. These properties may also include
#' `"row_height"` (for rbind) or `"col_width"` (for cbind). Numeric row heights and column widths
#' will be rescaled to 1.
#'
#' If `copy_cell_props` is `TRUE`, the default set of cell properties (everything but `colspan` and
#' `rowspan`, including row heights/column widths) will be copied.
#'
#' If `copy_cell_props` is `FALSE`, cells from non-huxtable objects will get the default properties.
#'
#' NB: You cannot bind huxtables with data frames, since the R method dispatch will always
#' call the data frame method instead of the huxtable-specific code. For a solution, see
#' [add_columns()].
#'
#'
#' @examples
#' ht1 <- hux(a = 1:3, b = 4:6)
#' ht2 <- hux(
#'         d = letters[1:3],
#'         e = letters[4:6]
#'       )
#' bold(ht1)[1, ] <- TRUE
#' bold(ht2) <- TRUE
#' vec <- LETTERS[1:3]
#'
#' cbind(ht1, vec, ht2)
#' cbind(ht1, vec, ht2,
#'       copy_cell_props = FALSE)
#'
#' @export
cbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  assert_that(is.flag(copy_cell_props) || is.character(copy_cell_props))
  bind_hux(..., type = "cbind", copy_cell_props = copy_cell_props)
}


#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  assert_that(is.flag(copy_cell_props) || is.character(copy_cell_props))
  bind_hux(..., type = "rbind", copy_cell_props = copy_cell_props)
}


bind_hux <- function (..., type, copy_cell_props) {
  default_copy_attrs <- c(
          setdiff(huxtable_cell_attrs, c("colspan", "rowspan")),
          switch(type, rbind = "row_height", cbind = "col_width")
        )
  if (isTRUE(copy_cell_props)) copy_cell_props <- default_copy_attrs
  objs <- list(...)
  arg_names <- names(sapply(substitute(list(...))[-1], deparse))

  objs <- lapply(seq_along(objs), function(idx) {
    x <- objs[[idx]]
    if (is.vector(x) || is.factor(x)) {
      x <- as.matrix(x)
      if (! is.null(arg_names) && nzchar(arg_names[idx])) colnames(x) <- arg_names[idx]
      if (type == "rbind") x <- t(x)
    }
    if (is.null(attr(x, "from_real_hux"))) attr(x, "from_real_hux") <- is_hux(x)
    x
  })

  f <- function (ht, x) bind2_hux(ht, x, type, copy_cell_props = copy_cell_props)
  res <- Reduce(f, objs)

  daddy <- Find(is_hux, objs)
  unchanged_attrs <- switch(type, "cbind" = huxtable_row_attrs, "rbind" = huxtable_col_attrs)
  for (a in c(unchanged_attrs, huxtable_table_attrs)) attr(res, a) <- attr(daddy, a)

  attr(res, "from_real_hux") <- NULL
  res
}


bind2_hux <- function (ht, x, type, copy_cell_props) {
  ht_real_hux <- attr(ht, "from_real_hux")
  x_real_hux  <- attr(x, "from_real_hux")

  ht <- as_hux(ht, autoformat = FALSE, add_colnames = FALSE)
  x  <- as_hux(x, autoformat = FALSE, add_colnames = FALSE)
  ccp <- intersect(copy_cell_props, huxtable_cell_attrs)

  if (is.character(ccp)) {
    if (! x_real_hux  && nrow(x) > 0 && ncol(x) > 0) {
      for (a in ccp) {
        attr(x, a)[] <- if (type == "cbind") attr(ht, a)[, ncol(ht)] else
          matrix(attr(ht, a)[nrow(ht), ], nrow(x), ncol(x), byrow = TRUE)
      }
      if ("row_height" %in% copy_cell_props && type == "rbind") {
        attr(x, "row_height") <- attr(ht, "row_height")[nrow(ht)]
      }
      if ("col_width" %in% copy_cell_props && type == "cbind") {
        attr(x, "col_width") <- attr(ht, "col_width")[ncol(ht)]
      }
    }
    if (! ht_real_hux && x_real_hux && nrow(ht) > 0 && ncol(ht) > 0) {
      for (a in ccp) {
        attr(ht, a)[] <- if (type == "cbind") attr(x, a)[, 1] else
          matrix(attr(x, a)[1, ], nrow(ht), ncol(ht), byrow = TRUE)
      }
      if ("row_height" %in% copy_cell_props && type == "rbind") {
        attr(ht, "row_height") <- attr(x, "row_height")[1]
      }
      if ("col_width" %in% copy_cell_props && type == "cbind") {
        attr(ht, "col_width") <- attr(x, "col_width")[1]
      }
    }
  }

  bind_df <- switch(type, "cbind" = cbind.data.frame, "rbind" = function (x, y) {
    if(ncol(x) != ncol(y)) stop("Can't rbind objects as they have different numbers of columns")
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })

  res <- as_hux(bind_df(ht, x), autoformat = FALSE, add_colnames = FALSE)
  res <- merge_props(res, ht, x, type = type, copy_cell_props = copy_cell_props)

  attr(res, "from_real_hux") <- x_real_hux || ht_real_hux
  res
}


delete_props <- function (res, idx, type = c("cols", "rows")) {
  if (is.logical(idx)) idx <- which(idx)
  type <- match.arg(type)

  if (type == "cols") {
    for (a in huxtable_col_attrs) {
      attr(res, a) <- attr(res, a)[ -idx]
    }
    for (a in huxtable_cell_attrs) {
      attr(res, a) <- attr(res, a)[, -idx, drop = FALSE]
    }
  } else {
    for (a in huxtable_row_attrs) {
      attr(res, a) <- attr(res, a)[ -idx]
    }
    for (a in huxtable_cell_attrs) {
      attr(res, a) <- attr(res, a)[-idx, , drop = FALSE]
    }
  }

  res
}


# returns res with properties created from 'first' and 'second' huxtables
merge_props <- function (res, first, second, type = c("cbind", "rbind"), copy_cell_props = FALSE) {
  type <- match.arg(type)
  # if second is not a huxtable, make it a huxtable; and if ccp is TRUE, copy properties over:
  #  - cell properties copied L-R from last col (cbind) or T-B from last row (rbind)
  #  - row  properties copied from last row (rbind)
  #  - col  properties copied from last col (cbind)
  if (! is_huxtable(second)) {
    second <- as_hux(second, add_colnames = FALSE, autoformat = FALSE)
    if (is.character(copy_cell_props)) {
      ccp <- intersect(copy_cell_props, huxtable_cell_attrs)
      for (a in ccp) {
        attr(second, a)[] <- if (type == "cbind") attr(first, a)[, ncol(first)] else
          matrix(attr(first, a)[nrow(first), ], nrow(second), ncol(second), byrow = TRUE)
      }
      if (type == "rbind") for (a in huxtable_row_attrs) {
        attr(second, a) <- rep(attr(first, a)[nrow(first)], nrow(second))
      }
      if (type == "cbind") for (a in huxtable_col_attrs) {
        attr(second, a) <- rep(attr(first, a)[ncol(first)], ncol(second))
      }
    }
  }
  # c- or rbind first and second's properties into res, as follows:
  #  - first gets priority for table properties;
  #  - all cell properties are just c- or rbinded
  #  - row properties are concatenated if type=='rbind', otherwise they are from `first`
  #  - col properties are concatenated if type=='cbind', otherwise they are from `first`

  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(first, a)
  }
  bind_cells <- switch(type, "cbind" = cbind, "rbind" = rbind)
  for (a in huxtable_cell_attrs) {
    attr(res, a) <- bind_cells(attr(first, a), attr(second, a))
  }
  join_attrs  <- switch(type, "cbind" = huxtable_col_attrs, "rbind" = huxtable_row_attrs)
  first_attrs <- switch(type, "cbind" = huxtable_row_attrs, "rbind" = huxtable_col_attrs)
  for (a in join_attrs) {
    attr(res, a) <- c(attr(first, a), attr(second, a))
  }
  for (a in first_attrs) {
    attr(res, a) <- attr(first, a)
  }

  # numeric row/col heights are rescaled to add to 1
  for (rh_cw in c("row_height", "col_width")) {
    if (is.numeric(attr(res, rh_cw))) {
      values <- attr(res, rh_cw)
      attr(res, rh_cw) <- values / sum(values)
    }
  }

  res
}


#' Transpose a huxtable
#'
#' @param x A huxtable.
#'
#' @return The transposed object.
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
  for (a in setdiff(huxtable_cell_attrs, c("colspan", "rowspan", "height", "width",
    "bottom_border", "left_border", "top_border", "right_border"))) {
    attr(res, a) <- t(attr(x, a))
  }
  attr(res, "colspan") <- t(attr(x, "rowspan"))
  attr(res, "rowspan") <- t(attr(x, "colspan"))
  attr(res, "width")   <- attr(x, "height")
  attr(res, "height")  <- attr(x, "width")
  attr(res, "bottom_border") <- t(attr(x, "right_border"))
  attr(res, "right_border")  <- t(attr(x, "bottom_border"))
  attr(res, "left_border")   <- t(attr(x, "top_border"))
  attr(res, "top_border")    <- t(attr(x, "left_border"))
  row_height(res) <- col_width(x)
  col_width(res)  <- row_height(x)
  rownames(res)   <- colnames(x)
  colnames(res)   <- rownames(x)
  for (a in huxtable_table_attrs) {
    attr(res, a) <- attr(x, a)
  }

  res
}


#' Add column or row names
#'
#' Add a first row of column names, or a first column of row names, to the huxtable.
#'
#' Note that `add_colnames` will change the mode of all columns to character. Also note that it will
#' move your rows down by one: what was row 1 will now be row 2, and the column names will now be row 1.
#'
#' `add_colnames` preserves column names. `add_rownames` only preserves them if asked to.
#'
#' @param ht A huxtable.
#' @param colname Column name for the new column of row names.
#' @param rowname Optional row name for the new row of column names.
#' @param preserve_rownames Preserve existing row names.
#' @param ... Arguments passed to methods.
#'
#' @return The modified object.
#'
#' @examples
#' ht <- huxtable(
#'         First  = rnorm(5),
#'         Second = rnorm(5)
#'       )
#' add_rownames(ht)
#' add_colnames(ht)
#'
#' # Out by 1:
#' add_rownames(add_colnames(ht))
#'
#' # Better:
#' add_colnames(add_rownames(ht))
#'
#' # Alternatively:
#' add_colnames(add_rownames(ht, ""))
#'
#' @export
add_colnames <- function (ht, ...) UseMethod("add_colnames")


#' @export
#' @rdname add_colnames
add_colnames.huxtable <- function (ht, rowname = NULL, ...) {
  if (! missing(rowname)) assert_that(is.null(rowname) || is.string(rowname))
  cn <- colnames(ht)
  dateish_cols <- which(sapply(ht, function (x) class(x)[1] %in% c("Date", "POSIXct", "POSIXlt")))
  for (col in dateish_cols) ht[[col]] <- as.character(ht[[col]]) # avoids autoconversion by c(),
  # which uses as.numeric
  ht <- rbind(cn, ht, copy_cell_props = FALSE)
  number_format(ht)[1, ] <- NA
  colnames(ht) <- cn
  if (! is.null(rowname)) rownames(ht) <- c(rowname, rownames(ht)[1:(nrow(ht) - 1)])

  ht
}


#' @export
#' @rdname add_colnames
add_rownames <- function (ht, ...) UseMethod("add_rownames")


#' @export
#' @rdname add_colnames
add_rownames.huxtable <- function (ht, colname = "rownames", preserve_rownames = TRUE, ...) {
  assert_that(is.string(colname))
  ht <- cbind(rownames(ht), ht, copy_cell_props = FALSE)
  number_format(ht)[, 1] <- NA
  colnames(ht)[1] <- colname
  if (! preserve_rownames) rownames(ht) <- NULL

  ht
}


#' @export
`dimnames<-.huxtable` <- function (x, value) {
  x <- NextMethod()
  x <- set_attr_dimnames(x)

  x
}


set_attr_dimnames <- function(ht) {
  for (a in huxtable_cell_attrs) {
    dimnames(attr(ht, a)) <- dimnames(ht)
  }
  for (a in huxtable_col_attrs) {
    names(attr(ht, a)) <- dimnames(ht)[[2]]
  }
  for (a in huxtable_row_attrs) {
    names(attr(ht, a)) <- dimnames(ht)[[1]]
  }

  ht
}
