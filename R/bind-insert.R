
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
    y <- as_hux(y, add_colnames = FALSE)
    attr(y, "from_real_hux") <- FALSE
  }
  if (is_hux(y) && just_a_data_frame(x)) {
    x <- as_hux(x, add_colnames = FALSE)
    attr(x, "from_real_hux") <- FALSE
  }
  dims <- dim(x)
  end_idx <- dims[dimno]
  assert_that(is.numeric(dims))
  if (is.character(after)) {
    after_n <- match(after, dimnames(x)[[dimno]])
    if (is.na(after_n)) stop("Could not find column name \"", after, "\" in huxtable")
    after <- after_n
  }
  assert_that(is.number(after), after >= 0, after <= end_idx)

  first_idxes <- seq_len(after)
  second_idxes <- if (after < end_idx) seq(after + 1, end_idx) else integer(0)
  # for some reason `fn <- if (dimno==1) rbind else cbind` causes trouble...

  has_dims <- function (x) if (is.vector(x)) length(x) > 0 else
        (nrow(x) > 0 && ncol(x) > 0)
  if (dimno == 1) {
    objs <- list(x[first_idxes, ], y, x[second_idxes, ])
    objs <- Filter(has_dims, objs)
    do.call(rbind, c(objs, ...))
  } else {
    objs <- list(x[, first_idxes], y, x[, second_idxes])
    objs <- Filter(has_dims, objs)
    do.call(cbind, c(objs, ...))
  }
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
#' this. The default `NULL` throws an error if there are too few elements.
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

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[, seq(1, after, 1)]
  }
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

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[seq(1, after, 1), ]
  }
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
#' vec <- LETTERS[1:4]
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
        attr(x, "row_height")[seq_len(nrow(x))] <- attr(ht, "row_height")[nrow(ht)]
      }
      if ("col_width" %in% copy_cell_props && type == "cbind") {
        attr(x, "col_width")[seq_len(ncol(x))] <- attr(ht, "col_width")[ncol(ht)]
      }
    }
    if (! ht_real_hux && x_real_hux && nrow(ht) > 0 && ncol(ht) > 0) {
      for (a in ccp) {
        attr(ht, a)[] <- if (type == "cbind") attr(x, a)[, 1] else
          matrix(attr(x, a)[1, ], nrow(ht), ncol(ht), byrow = TRUE)
      }
      if ("row_height" %in% copy_cell_props && type == "rbind") {
        attr(ht, "row_height")[seq_len(nrow(ht))] <- attr(x, "row_height")[1]
      }
      if ("col_width" %in% copy_cell_props && type == "cbind") {
        attr(ht, "col_width")[seq_len(ncol(ht))] <- attr(x, "col_width")[1]
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
  header_rows(ht)[1] <- TRUE
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
  header_cols(ht)[1] <- TRUE
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
