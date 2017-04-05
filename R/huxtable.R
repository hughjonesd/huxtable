
#' Huxtable: simply create LaTeX and HTML tables
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @details
#'
#' To create a huxtable object, use \code{\link{huxtable}} or \code{\link{as_huxtable}}.
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website} or
#' read the vignette with \code{vignette('huxtable')}.
#'
#' @section Package options:
#'
#' \itemize{
#'   \item \code{options('huxtable.print')} sets the print method for huxtable objects.
#' }
#'
#' On the console, by default, huxtables print using the \code{\link{print_screen}} method. Within a knitr chunk,
#' huxtables will automatically print using the appropriate method for the document,
#' so that you can just write, e.g. \code{ht} rather than \code{print_html(ht)}.
#' The \code{huxtable.print} option lets you control the default output of printing huxtables,
#' in cases where this autodetection doesn't work: e.g. set
#' \code{options(huxtable.print = print_html)}.
#'
#' @name huxtable-package
NULL

#' @import stats
#' @import grDevices

#'
NULL


#' Create a huxtable
#'
#' \code{huxtable}, or \code{hux}, creates a huxtable object.
#'
#' @param ... Named list of values, as for \code{\link{data.frame}}.
#' @param add_colnames If \code{TRUE}, add a first row of column names to the huxtable.
#' @param add_rownames If \code{TRUE}, add a first column of row names, named 'rownames', to the huxtable.
#'
#' @return An object of class \code{huxtable}.
#' @export
#'
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
huxtable <- function (..., add_colnames = FALSE, add_rownames = FALSE) {
  ht <- data.frame(..., stringsAsFactors = FALSE)
  ht <- as_huxtable(ht, add_colnames = add_colnames, add_rownames = add_rownames)

  ht
}

#' @export
#' @rdname huxtable
hux <- huxtable


#' @param x An object to convert to a huxtable.
#'
#' @export
#' @details
#' \code{as_huxtable} and \code{as_hux} convert an object to a huxtable.
#' Conversion methods exist for data frames, tables, ftables, matrices and (most) vectors.
#' @examples
#' dfr <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
#' as_huxtable(dfr)
#'
#' @rdname huxtable
as_huxtable <- function (x, ...) UseMethod('as_huxtable')

#' @export
#' @rdname huxtable
as_hux <- as_huxtable

#' @export
#' @rdname huxtable
as_huxtable.default <- function (x, add_colnames = FALSE, add_rownames = FALSE, ...) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (att in setdiff(huxtable_cell_attrs, 'number_format')) {
    attr(x, att) <- matrix(NA, nrow(x), ncol(x))
  }
  for (att in huxtable_col_attrs) {
    attr(x, att) <- rep(NA, ncol(x))
  }
  for (att in huxtable_row_attrs) {
    attr(x, att) <- rep(NA, nrow(x))
  }
  for (att in huxtable_table_attrs) {
    attr(x, att) <- NA
  }
  attr(x, 'number_format') <- matrix(list(NA), nrow(x), ncol(x))
  for (att in names(huxtable_default_attrs)) {
    attr(x, att)[] <- huxtable_default_attrs[[att]] # [[ indexing matters here
  }

  class(x) <- c('huxtable', class(x))

  # order matters here. We want original rownames, not anything else.
  if (add_rownames) x <- add_rownames(x, preserve_rownames = FALSE)
  if (add_colnames) x <- add_colnames(x)

  x <- set_attr_dimnames(x)
  x
}

#' @export
as_huxtable.huxtable <- function (x, ...) x

#' @export
as_huxtable.table <- function (x, add_colnames = TRUE, add_rownames = TRUE, ...) {
  ht <- as_huxtable(unclass(x), add_colnames, add_rownames, ...)
  number_format(ht) <- 0
  ht
}

#' @export
as_huxtable.ftable <- function(x, ...) {
  ht <- as_huxtable(format(x, quote = FALSE), ...)
  number_format(ht) <- 0
  ht
}


#' @export
as_huxtable.numeric <- function (x, ...) {
  # use default otherwise matrix has class e.g. c('matrix', 'numeric') so we recurse
  as_huxtable.default(as.matrix(x), ...)
}

#' @export
as_huxtable.character <- as_huxtable.numeric

#' @export
as_huxtable.logical   <- as_huxtable.numeric

#' @export
as_huxtable.complex   <- as_huxtable.numeric

#' @export
#' @rdname huxtable
is_huxtable <- function (x) inherits(x, 'huxtable')

#' @export
#' @rdname huxtable
is_hux <- is_huxtable

#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j Columns to select.
#' @param drop Not used.
#'
#' @return A huxtable.
#' @export
#' @rdname extract-methods
#' @details
#' \code{[} always returns a new huxtable object, while \code{$} and \code{[[} simply
#' return a vector of data.
#' For the replacement function \code{[<-}, if \code{value} is a huxtable, then its cell properties will be
#' copied into \code{x}. In addition, if \code{nrow(value) == nrow(x)}, then column properties
#' will be copied into the replaced columns of \code{x}, and if  \code{ncol(value) == ncol(x)}, then
#' row properties will be copied into the replaced rows.
#' Replacement functions \code{$<-} and \code{[[<-} simply change the data without affecting other properties.
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' ht[1:2,]
#' ht[,1]
#' ht$a
#' \dontrun{
#' rowspan(ht)[2,1] <- 2
#' ht[1:2,] # generates a warning
#' }
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- as.data.frame(x)[i, j, drop]
  if (! missing(i) && is.character(i)) i <- which(rownames(x) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(x) %in% j)
  for (att in huxtable_cell_attrs) {
    attr(ss, att) <- attr(x, att)[i, j, drop = drop]
  }
  for (att in huxtable_col_attrs) {
    attr(ss, att) <- attr(x, att)[j]
  }
  for (att in huxtable_row_attrs) {
    attr(ss, att) <- attr(x, att)[i]
  }
  for (att in huxtable_table_attrs) {
    attr(ss, att) <- attr(x, att)
  }

  class(ss) <- class(x)
  colspan(ss) <- pmin(colspan(ss), 1 + ncol(ss) - col(ss))
  rowspan(ss) <- pmin(rowspan(ss), 1 + nrow(ss) - row(ss))

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
#' bold(ht)
#'
`[<-.huxtable` <- function (x, i, j, value) {
  res <- as.data.frame(NextMethod())

  if (ncol(res) < ncol(x)) {
    stopifnot(is.null(value))
    # could be ht[,'foo'] <- NULL or ht['foo'] <- NULL so this is safest:
    idx <- which(! colnames(x) %in% colnames(res))
    res <- delete_props(res, idx, type = 'cols')
  }
  if (ncol(res) > ncol(x)) {
    # Assumption: extra columns are on the right. Note that we may ALSO have more rows
    res <- merge_props(res, x, res[seq_len(nrow(x)), seq(ncol(x) + 1, ncol(res))], type = 'cbind')
  }
  if (nrow(res) > nrow(x)) {
    # we may have already given res the appropriate attributes above; if so use them since x has too few cols
    first <- if (ncol(res) > ncol(x)) res[seq_len(nrow(x)),] else x
    res <- merge_props(res, first, res[seq(nrow(x) + 1, nrow(res)), ], type = 'rbind')
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


#' @param name See \code{\link{Extract.data.frame}}.
#'
#' @rdname extract-methods
#' @export
`$<-.huxtable` <- function (x, name, value) {
  res <- as.data.frame(NextMethod())

  if (ncol(res) < ncol(x)) {
    stopifnot(is.null(value))
    idx <- if (is.character(name)) match(name, colnames(x)) else name
    res <- delete_props(res, idx, type = 'cols')
  }
  if (ncol(res) > ncol(x)) {
    res <- merge_props(res, x, res[, seq(ncol(x) + 1, ncol(res))], type = 'cbind')
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
    res <- delete_props(res, idx, type = 'cols')
  }
  if (ncol(res) > ncol(x)) {
    # Assumption: extra columns are on the right
    res <- merge_props(res, x, res[, seq(ncol(x) + 1, ncol(res))], type = 'cbind')
  }
  if (nrow(res) > nrow(x)) {
    res <- merge_props(res, x, res[seq(nrow(x) + 1, nrow(res)), ], type = 'rbind')
  }

  res <- set_attr_dimnames(res)
  class(res) <- class(x)
  res
}

#' Combine rows or columns
#'
#' @param ... Vectors, matrices, data frames or huxtables.
#' @param deparse.level Passed to \code{\link{cbind.data.frame}}.
#' @param copy_cell_props Cell properties to copy from neighbours (see below).
#'
#' @return A huxtable.
#'
#' @details
#' Table properties will be taken from the first argument which is a huxtable. So will
#' row properties (for cbind) and column properties (for rbind).
#'
#' If some of the inputs are not huxtables, and \code{copy_cell_props} is a character vector of cell properties,
#' then for rbind, the named cell properties and row heights will be copied to non-huxtables. For cbind,
#' the named cell properties and column widths will be copied. Objects on the left or above get priority
#' over those on the right or below.
#'
#' If \code{copy_cell_props} is \code{TRUE}, the default
#' set of cell properties (everything but \code{colspan} and \code{rowspan}) will be copied.
#'
#' If \code{copy_cell_props} is \code{FALSE}, cells from non-huxtable objects will get the
#' default properties.
#'
#' @examples
#' ht1 <- hux(a = 1:3, b = 4:6)
#' ht2 <- hux(d = letters[1:3], e = letters[4:6])
#' bold(ht1)[1,] <- TRUE
#' bold(ht2) <- TRUE
#' vec <- LETTERS[1:3]
#'
#' ht_out <- cbind(ht1, vec, ht2)
#' ht_out
#' bold(ht_out)
#' bold(cbind(ht1, vec, ht2, copy_cell_props = FALSE))
#'
#' @export
cbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  bind_hux(..., type = 'cbind', copy_cell_props = copy_cell_props)
}


#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function (..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  bind_hux(..., type = 'rbind', copy_cell_props = copy_cell_props)
}


bind_hux <- function (..., type, copy_cell_props) {
  default_copy_attrs <- setdiff(huxtable_cell_attrs, c('colspan', 'rowspan'))
  if (isTRUE(copy_cell_props)) copy_cell_props <- default_copy_attrs
  objs <- list(...)
  arg_names <- names(sapply(substitute(list(...))[-1], deparse))

  objs <- lapply(seq_along(objs), function(idx) {
    x <- objs[[idx]]
    if (is.vector(x) || is.factor(x)) {
      x <- as.matrix(x)
      if (! is.null(arg_names) && nzchar(arg_names[idx])) colnames(x) <- arg_names[idx]
      if (type == 'rbind') x <- t(x)
    }
    attr(x, 'from_real_hux') <- is_hux(x)
    x
  })

  f <- function (ht, x) bind2_hux(ht, x, type, copy_cell_props = copy_cell_props)
  res <- Reduce(f, objs)

  daddy <- Find(is_hux, objs)
  first_attrs <- switch(type, 'cbind' = huxtable_row_attrs, 'rbind' = huxtable_col_attrs)
  for (att in c(first_attrs, huxtable_table_attrs)) attr(res, att) <- attr(daddy, att)

  attr(res, 'from_real_hux') <- NULL
  res
}


bind2_hux <- function (ht, x, type, copy_cell_props) {
  ht_real_hux <- attr(ht, 'from_real_hux')
  x_real_hux  <- attr(x, 'from_real_hux')

  ht <- as_hux(ht)
  x  <- as_hux(x)
  ccp <- intersect(copy_cell_props, huxtable_cell_attrs)

  if (is.character(ccp)) {
    if (! x_real_hux) {
      for (att in ccp) {
        attr(x, att)[] <- if (type == 'cbind') attr(ht, att)[, ncol(ht)] else
          matrix(attr(ht, att)[nrow(ht), ], nrow(x), ncol(x), byrow = TRUE)
      }
    }
    if (! ht_real_hux && x_real_hux) {
      for (att in ccp) {
        attr(ht, att)[] <- if (type == 'cbind') attr(x, att)[, 1] else
          matrix(attr(x, att)[1, ], nrow(ht), ncol(ht), byrow = TRUE)
      }
    }
  }

  bind_df <- switch(type, 'cbind' = cbind.data.frame, 'rbind' = function (x, y) {
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })
  bind_cells <- switch(type, 'cbind' = cbind, 'rbind' = rbind)

  res <- as_hux(bind_df(ht, x))
  for (att in huxtable_cell_attrs) {
    attr(res, att) <- bind_cells(attr(ht, att), attr(x, att))
  }
  join_attrs <- switch(type, 'cbind' = huxtable_col_attrs, 'rbind' = huxtable_row_attrs)
  for (att in join_attrs) {
    attr(res, att) <- c(attr(ht, att), attr(x, att))
  }

  attr(res, 'from_real_hux') <- x_real_hux || ht_real_hux
  res
}


delete_props <- function (res, idx, type = c('cols', 'rows')) {
  if (is.logical(idx)) idx <- which(idx)
  type <- match.arg(type)

  if (type == 'cols') {
    for (a in huxtable_col_attrs) {
      attr(res, a) <- attr(res, a)[ -idx]
    }
    for (a in huxtable_cell_attrs) {
      attr(res, a) <- attr(res, a)[ , -idx, drop = FALSE]
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


merge_props <- function (res, first, second, type = c('cbind', 'rbind'), copy_cell_props = TRUE) {
  type <- match.arg(type)
  # if second is not a huxtable, make it a huxtable; and if ccp is TRUE, copy properties over:
  #  - cell properties copied L-R from last col (cbind) or T-B from last row (rbind)
  #  - row  properties copied from last row (rbind)
  #  - col  properties copied from last col (cbind)
  if (! is_huxtable(second)) {
    second <- as_hux(second)
    if (isTRUE(copy_cell_props)) {
      cell_attrs_to_copy <- setdiff(huxtable_cell_attrs, c('colspan', 'rowspan'))
      for (att in cell_attrs_to_copy) {
        attr(second, att)[] <- if (type == 'cbind') attr(first, att)[, ncol(first)] else
              matrix(attr(first, att)[nrow(first), ], nrow(second), ncol(second), byrow = TRUE)
      }
      if (type == 'rbind') for (att in huxtable_row_attrs) {
        attr(second, att) <- rep(attr(first, att)[nrow(first)], nrow(second))
      }
      if (type == 'cbind') for (att in huxtable_col_attrs) {
        attr(second, att) <- rep(attr(first, att)[ncol(first)], ncol(second))
      }
    }
  }
  # c or rbind first and second's properties into res, as follows:
  #  - first gets priority for table properties;
  #  - all cell properties are just c or rbinded
  #  - row properties are concatenated if type=='rbind', otherwise they are from `first`
  #  - col properties are concatenated if type=='cbind', otherwise they are from `first`

  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(first, att)
  }
  bind_cells <- switch(type, 'cbind' = cbind, 'rbind' = rbind)
  for (att in huxtable_cell_attrs) {
    attr(res, att) <- bind_cells(attr(first, att), attr(second, att))
  }
  join_attrs  <- switch(type, 'cbind' = huxtable_col_attrs, 'rbind' = huxtable_row_attrs)
  first_attrs <- switch(type, 'cbind' = huxtable_row_attrs, 'rbind' = huxtable_col_attrs)
  for (att in join_attrs) {
    attr(res, att) <- c(attr(first, att), attr(second, att))
  }
  for (att in first_attrs) {
    attr(res, att) <- attr(first, att)
  }

  res
}


#' @importFrom dplyr filter_
#' @export
filter_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- as.data.frame(.data)
  .data <- tibble::rownames_to_column(.data, 'filter.huxtable.rownames')
  result <- NextMethod()
  ht[.data$filter.huxtable.rownames %in% result$filter.huxtable.rownames, ]
}

#' Dplyr verbs for huxtable
#'
#' Huxtable can be used with dplyr verbs \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}},
#' \code{\link[dplyr]{slice}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{mutate}} and
#' \code{\link[dplyr]{transmute}}. These will return huxtables. Other verbs like \code{\link[dplyr]{summarize}}
#' will simply return data frames as normal.
#'
#' @param .data A huxtable.
#' @param ...,.dots Arguments passed to \code{\link[dplyr]{mutate}}.
#'
#' @details
#' If \code{mutate} creates new columns, and the argument \code{copy_cell_props} is missing or \code{TRUE}, then cell
#' and column properties will be copied from existing columns to their left, if there are any. Otherwise, they will be the
#' standard defaults. Row and table properties, and properties of cells in existing columns, remain unchanged.
#'
#' @rdname dplyr-verbs
#' @aliases mutate
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, c = 1:5, d = 1:5)
#' bold(ht)[c(1, 3), ] <- TRUE
#' bold(ht)[, 1] <- TRUE
#' ht2 <- dplyr::select(ht, b:c)
#' ht2
#' bold(ht2)
#' ht3 <- dplyr::mutate(ht, x = a + b)
#' ht3
#' bold(ht3)
#' ht4 <- dplyr::mutate(ht, x = a + b, copy_cell_props = FALSE)
#' bold(ht4)
#' @importFrom dplyr mutate_
#' @export
mutate_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- as.data.frame(.data)
  copy_cell_props <- TRUE
  if (! is.null(.dots$copy_cell_props)) copy_cell_props <- lazyeval::lazy_eval(.dots$copy_cell_props)
  .dots <- .dots[setdiff(names(.dots), 'copy_cell_props')]
  result <- NextMethod()
  result <- as_hux(result)

  for (att in c(huxtable_row_attrs, huxtable_table_attrs)) {
    attr(result, att) <- attr(ht, att)
  }

  match_cols <- match(colnames(result), colnames(ht))
  if (copy_cell_props) match_cols <- Reduce(function (x, y) if (is.na(y)) x else y, match_cols, accumulate = TRUE)
  result_cols <- ! is.na(match_cols)
  match_cols  <- na.omit(match_cols)

  for (att in huxtable_cell_attrs) attr(result, att)[, result_cols] <- attr(ht, att)[, match_cols]
  for (att in huxtable_col_attrs)  attr(result, att)[result_cols]  <- attr(ht, att)[match_cols]

  result <- set_attr_dimnames(result)

  result
}


#' @importFrom dplyr transmute_
#' @export
transmute_.huxtable <- mutate_.huxtable

#' @importFrom dplyr arrange_
#' @export
arrange_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- tibble::rownames_to_column(.data, 'arrange.huxtable.rownames')
  result <- NextMethod()
  ht[match(result$arrange.huxtable.rownames, .data$arrange.huxtable.rownames), ]
}


#' @importFrom dplyr slice_
#' @export
slice_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- tibble::rownames_to_column(.data, 'slice.huxtable.rownames')
  result <- NextMethod()
  ht[na.omit(match(result$slice.huxtable.rownames, .data$slice.huxtable.rownames)), ]
}


#' @importFrom dplyr select_
#' @export
select_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- as.data.frame(t(colnames(.data)), stringsAsFactors = FALSE)
  colnames(.data) <- colnames(ht)
  result <- NextMethod()
  ht <- ht[, na.omit(match(result[1, ], colnames(ht)))]
  colnames(ht) <- colnames(result)

  ht
}


#' @importFrom dplyr rename_
#' @export
rename_.huxtable <- select_.huxtable

#' @export
`dimnames<-.huxtable` <- function (x, value) {
  x <- NextMethod()
  x <- set_attr_dimnames(x)
  x
}

#' Transpose a huxtable
#'
#' @param x A huxtable.
#'
#' @return The transposed object.
#'
#' @details
#' Row and column spans of \code{x} will be swapped, as will column widths and row heights,
#' table width and height, and cell borders (bottom becomes right, etc.).
#' Other properties - in particular, alignment, vertical alignment and rotation - will be
#' preserved.
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' bottom_border(ht)[3,] <- 1
#' t(ht)
#'
#' @export
t.huxtable <- function (x) {
  res <- as_hux(NextMethod())
  for (att in setdiff(huxtable_cell_attrs, c('colspan', 'rowspan', 'height', 'width',
        'bottom_border', 'left_border', 'top_border', 'right_border'))) {
    attr(res, att) <- t(attr(x, att))
  }
  attr(res, 'colspan') <- t(attr(x, 'rowspan'))
  attr(res, 'rowspan') <- t(attr(x, 'colspan'))
  attr(res, 'width')   <- attr(x, 'height')
  attr(res, 'height')  <- attr(x, 'width')
  attr(res, 'bottom_border') <- t(attr(x, 'right_border'))
  attr(res, 'right_border')  <- t(attr(x, 'bottom_border'))
  attr(res, 'left_border')   <- t(attr(x, 'top_border'))
  attr(res, 'top_border')    <- t(attr(x, 'left_border'))
  row_height(res) <- col_width(x)
  col_width(res)  <- row_height(x)
  rownames(res)   <- colnames(x)
  colnames(res)   <- rownames(x)
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(x, att)
  }

  res
}




#' Add column or row names
#'
#' Add a first row of column names, or a first column of row names, to the huxtable.
#'
#' Note that \code{add_colnames} will change the mode of all columns to character.

#' \code{add_colnames} preserves column names. \code{add_rownames} only preserves them if asked to.
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
#' ht <- huxtable(a = 1:5, b = 1:5)
#' add_rownames(ht)
#' add_colnames(ht)
#' add_rownames(add_colnames(ht)) # Out by 1
#' add_colnames(add_rownames(ht)) # Better
#' add_colnames(add_rownames(ht, '')) # Alternatively
#'
#' @export
add_colnames <- function (ht, ...) UseMethod('add_colnames')

#' @export
#' @rdname add_colnames
add_colnames.huxtable <- function (ht, rowname = NULL, ...) {
  cn <- colnames(ht)
  ht <- rbind(cn, ht, copy_cell_props = FALSE)
  colnames(ht) <- cn
  if (! missing(rowname)) rownames(ht) <- c(rowname, rownames(ht)[1:(nrow(ht) - 1)])
  ht
}

#' @export
#' @rdname add_colnames
add_rownames <- function (ht, ...) UseMethod('add_rownames')

#' @export
#' @rdname add_colnames
add_rownames.huxtable <- function (ht, colname = 'rownames', preserve_rownames = TRUE, ...) {
  ht <- cbind(rownames(ht), ht, copy_cell_props = FALSE)
  colnames(ht)[1] <- colname
  if (! preserve_rownames) rownames(ht) <- NULL
  ht
}

set_attr_dimnames <- function(ht) {
  for (att in huxtable_cell_attrs) {
    dimnames(attr(ht, att)) <- dimnames(ht)
  }
  for (att in huxtable_col_attrs) {
    names(attr(ht, att)) <- dimnames(ht)[[2]]
  }
  for (att in huxtable_row_attrs) {
    names(attr(ht, att)) <- dimnames(ht)[[1]]
  }

  ht
}
