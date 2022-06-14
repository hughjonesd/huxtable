
#' @import assertthat
NULL


#' Create a huxtable
#'
#' `huxtable`, or `hux`, creates a huxtable object.
#'
#' @param ... For `huxtable`, named list of values as in [data.frame()].
#'   For `tribble_hux`, data values as in [tibble::tribble()].
#' @param add_colnames If `TRUE`, add a first row of column names to the huxtable.
#' @param add_rownames If `TRUE` or a character string, add a first column of row names
#'   to the huxtable. The string gives the name for the new column (or `"rownames"` for `TRUE`).
#' @param autoformat If `TRUE`, automatically format columns by type. See below.
#'
#' @return An object of class `huxtable`.
#' @export
#' @section Automatic formatting:
#'
#' If `autoformat` is `TRUE`, then columns will have [number_format()] and [align()] properties
#' set automatically, as follows:
#'
#' * Integer columns will have `number_format` set to 0.
#' * Other numeric columns will have `number_format` set to \code{"\%.3g"}.
#' * All other columns will have `number_format` set to `NA` (no formatting).
#' * Integer, `Date` and date-time (i.e. `POSIXct` and `POSIXlt`) columns will be right-aligned.
#' * Other numeric columns will be aligned on `options("OutDec")`, usually `"."`.
#' * Other columns will be left aligned.
#'
#' You can change these defaults by editing `options("huxtable.autoformat_number_format")` and
#' `options("huxtable.autoformat_align")`. See [huxtable-package] for more details.
#'
#' Automatic alignment also applies to column headers if `add_colnames` is `TRUE`; headers of
#' columns aligned on a decimal point will be right-aligned. Automatic number formatting does not
#' apply to column headers.
#'
#' @details
#' If you use `add_colnames` or `add_rownames`, be aware that these will shift your rows and columns
#' along by one: your old row/column 1 will now be row/column 2, etc.
#'
#' `add_colnames` defaults to `TRUE`. You can set the default globally by
#' setting `options("huxtable.add_colnames")` to `TRUE` or `FALSE`.
#'
#' @seealso [huxtable-options]
#'
#' @examples
#' ht <- huxtable(
#'         column1 = 1:5,
#'         column2 = letters[1:5]
#'       )
#' ht
huxtable <- function (
        ...,
        add_colnames = getOption("huxtable.add_colnames", TRUE),
        add_rownames = FALSE,
        autoformat   = getOption("huxtable.autoformat", TRUE)
      ) {

  assert_that(
          is.flag(add_colnames),
          is.flag(add_rownames) | is.string(add_rownames),
          is.flag(autoformat)
        )

  df_args <- list(..., stringsAsFactors = FALSE, check.names = FALSE)
  if (R.version$major >= 3 && R.version$minor >= 3) df_args$fix.empty.names <- FALSE
  ht <- do.call(data.frame, df_args)
  if (is.null(names(list(...))) & ! is.data.frame(..1)) add_colnames <- FALSE
  ht <- as_huxtable(ht, add_colnames = add_colnames, add_rownames = add_rownames,
        autoformat = autoformat)

  ht
}


#' @export
#' @rdname huxtable
hux <- huxtable


#' @export
#' @details
#' `tribble_hux` is a simple wrapper around `tibble::tribble()` which lets you
#'  create data in a readable format. It requires the "tibble" package to
#'  be installed.
#'
#' @rdname huxtable
#' @examples
#' tribble_hux(
#'   ~ Name,             ~ Salary,
#'     "John Smith",       50000,
#'     "Jane Doe",         50000,
#'     "David Hugh-Jones", 50000,
#'     add_colnames = TRUE
#' )
tribble_hux <- function (...,
        add_colnames = getOption("huxtable.add_colnames", TRUE),
        autoformat   = getOption("huxtable.autoformat", TRUE)
      ) {
  assert_package("tribble_hux", "tibble")
  as_hux(tibble::tribble(...), add_colnames = add_colnames, add_rownames = FALSE,
        autoformat = autoformat)
}


#' Convert objects to huxtables
#'
#' `as_huxtable` or `as_hux` converts an object to a huxtable.
#' Conversion methods exist for data frames and tibbles, tables,
#' ftables, matrices and (most) vectors.
#'
#' `is_hux[table]` tests if an object is a huxtable.
#'
#' @param x Object to convert.
#' @param ... Arguments passed on to [huxtable()].
#' @inheritParams huxtable
#'
#' @return An object of class "huxtable".
#'
#' @details
#' For `table` objects, `add_colnames` and `add_rownames` are `TRUE` by default. For
#' `matrix` objects, they are `FALSE`. Other classes use
#' `options("huxtable.add_colnames")`, which is `TRUE` by default; `add_rownames`
#' is `FALSE`.
#'
#' For [dplyr::grouped_df()] objects, groups will be converted to header rows
#' if `groups_to_header` is `TRUE`.
#'
#' @export
#' @examples
#' dfr <- data.frame(
#'         a = 1:5,
#'         b = letters[1:5],
#'         stringsAsFactors = FALSE
#'       )
#' as_huxtable(dfr)
#' mx <- matrix(letters[1:12], 4, 3)
#' as_huxtable(mx, add_colnames = FALSE)
#' library(stats)
#' tbl <- table(
#'         Wool    = warpbreaks$wool,
#'         Tension = warpbreaks$tension
#'       )
#' as_huxtable(tbl) # adds row and column names by default
#'
#' # adding rownames:
#' as_hux(mtcars[1:3,], add_colnames = TRUE,
#'       add_rownames = "Car")
#'
#' if (requireNamespace("dplyr")) {
#'   iris_grp <- dplyr::group_by(iris[c(1:4, 51:54, 101:104), ], Species)
#'   as_hux(iris_grp, groups_to_headers = TRUE)
#' }
as_huxtable <- function (x, ...) UseMethod("as_huxtable")


#' @export
#' @rdname as_huxtable
as_hux <- as_huxtable


#' @export
#' @rdname as_huxtable
as_huxtable.default <- function (
        x,
        add_colnames = getOption("huxtable.add_colnames", TRUE),
        add_rownames = FALSE,
        autoformat   = getOption("huxtable.autoformat", TRUE),
        ...
      ) {
  assert_that(
          is.flag(add_colnames),
          is.flag(add_rownames) || is.character(add_rownames),
          is.flag(autoformat)
        )

  x <- new_huxtable(x)

  col_classes <- sapply(x, function (col) class(col)[1])
  if (autoformat) {
    dfn <- getOption("huxtable.autoformat_number_format", list())
    for (cn in seq_len(ncol(x))) {
      # double [[ matters for getting underlying object; also want only most specific class:
      cls <- col_classes[cn]
      number_format(x)[, cn] <- dfn[[cls]] %||% NA
    }
  }

  # order matters here. We want original rownames, not anything else.
  if (is.character(add_rownames)) {
    rownames_colname <- add_rownames
    add_rownames <- TRUE
  } else {
    rownames_colname <- "rownames"
  }
  if (add_rownames) x <- add_rownames(x, preserve_rownames = FALSE, colname = rownames_colname)
  if (add_colnames) x <- add_colnames(x)
  # this bit comes after add_colnames so that column headers also get aligned:
  if (autoformat) {
    dfa <- getOption("huxtable.autoformat_align", list())
    for (cn in seq_len(ncol(x))) {
      cls <- col_classes[cn]
      autoal <- dfa[[cls]] %||% NA
      align(x)[, cn] <- autoal
      if (add_colnames && ! autoal %in% c("left", "right", "center", "centre", NA)) {
        align(x)[1, cn] <- "right"
      }
    }
  }

  x <- set_attr_dimnames(x)
  x
}


#' @export
as_huxtable.huxtable <- function (x, ...) x


#' @export
as_huxtable.matrix <- function(x, add_colnames = FALSE, ...) {
  as_huxtable.default(x, add_colnames = add_colnames, ...)
}


#' @export
as_huxtable.table <- function (x, add_colnames = TRUE, add_rownames = TRUE, ...) {
  ht <- as_huxtable(unclass(x), add_colnames, add_rownames, ...)
  if (add_rownames) {
    ht[1, 1] <- ""
  }

  ht
}


#' @export
as_huxtable.ftable <- function(x, ...) {
  ht <- as_huxtable(format(x, quote = FALSE), ...)
  number_format(ht) <- 0
  ht
}


#' @export
#' @param groups_to_header Logical. Convert groups to header rows?
as_huxtable.grouped_df <- function (x, groups_to_headers = FALSE) {
  assert_that(is.flag(groups_to_headers))

  ht <- NextMethod()
  if (groups_to_headers) {
    assert_package("as_huxtable.grouped_df", "dplyr")
    group_vars <- dplyr::group_vars(x)
    for (col in group_vars) {
      ht <- column_to_header(ht, col, glue = paste0(col, ": {value}"))
    }
  }

  ht
}


#' @export
as_huxtable.numeric <- function (x, ...) {
  # use default otherwise matrix has class e.g. c("matrix", "numeric") so we recurse
  as_huxtable.default(as.matrix(x), ...)
}


#' @export
as_huxtable.character <- as_huxtable.numeric


#' @export
as_huxtable.logical   <- as_huxtable.numeric


#' @export
as_huxtable.complex   <- as_huxtable.numeric


#' @export
#' @rdname as_huxtable
is_huxtable <- function (x) inherits(x, "huxtable")


#' @export
#' @rdname as_huxtable
is_hux <- is_huxtable
