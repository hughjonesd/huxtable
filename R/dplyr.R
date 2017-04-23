
#' @importFrom dplyr filter_
#' @export
filter_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- as.data.frame(.data)
  .data <- tibble::rownames_to_column(.data, 'filter.huxtable.rownames')
  result <- NextMethod()
  ht[.data$filter.huxtable.rownames %in% result$filter.huxtable.rownames, ]
}

#' @export filter
NULL

#' @importFrom dplyr filter
#' @export
filter.huxtable <- function(.data, ...) {}
body(filter.huxtable) <- body(filter_.huxtable)


#' @importFrom dplyr mutate_
#' @export
mutate_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- as.data.frame(.data)
  copy_cell_props <- TRUE
  if (! is.null(.dots$copy_cell_props)) {
    if (utils::packageVersion("dplyr") > "0.5.0") {
      copy_cell_props <- .dots$copy_cell_props
    } else {
      copy_cell_props <- lazyeval::lazy_eval(.dots$copy_cell_props)
    }
  }
  .dots <- .dots[setdiff(names(.dots), 'copy_cell_props')]
  result <- NextMethod()
  result <- as_hux(result)

  for (a in c(huxtable_row_attrs, huxtable_table_attrs)) attr(result, a) <- attr(ht, a)

  # unlike in extract-methods we can't assume that new columns are on the right: transmute can reorder them
  # columns may even be reordered by e.g. a=NULL,...,a=new_value
  # so: all columns with an old name get the old attributes. New columns get copied attributes maybe.
  match_cols <- match(colnames(result), colnames(ht))
  if (copy_cell_props) match_cols <- Reduce(function (x, y) if (is.na(y)) x else y, match_cols, accumulate = TRUE)
  result_cols <- ! is.na(match_cols)
  match_cols  <- na.omit(match_cols)

  for (a in huxtable_cell_attrs) attr(result, a)[, result_cols] <- attr(ht, a)[, match_cols]
  for (a in huxtable_col_attrs)  attr(result, a)[result_cols]  <- attr(ht, a)[match_cols]

  result <- set_attr_dimnames(result)

  result
}

#' Dplyr verbs for huxtable
#'
#' Huxtable can be used with dplyr verbs \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}},
#' \code{\link[dplyr]{slice}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{mutate}} and
#' \code{\link[dplyr]{transmute}}. These will return huxtables. Other verbs like \code{\link[dplyr]{summarize}}
#' will simply return data frames as normal.
#'
#' @param .data A huxtable.
#' @param ... Arguments passed to \code{\link[dplyr]{mutate}}.
#' @param copy_cell_props Logical: copy cell and column properties from existing columns.
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
#' @importFrom dplyr mutate
#' @export
mutate.huxtable <- function (.data, ..., copy_cell_props = TRUE) {
  ht <- .data
  .data <- as.data.frame(.data)

  result <- NextMethod()
  result <- as_hux(result)

  for (a in c(huxtable_row_attrs, huxtable_table_attrs)) attr(result, a) <- attr(ht, a)

  match_cols <- match(colnames(result), colnames(ht))
  if (copy_cell_props) match_cols <- Reduce(function (x, y) if (is.na(y)) x else y, match_cols, accumulate = TRUE)
  result_cols <- ! is.na(match_cols)
  match_cols  <- na.omit(match_cols)

  for (a in huxtable_cell_attrs) attr(result, a)[, result_cols] <- attr(ht, a)[, match_cols]
  for (a in huxtable_col_attrs)  attr(result, a)[result_cols]  <- attr(ht, a)[match_cols]

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


#' @importFrom dplyr arrange
#' @export
arrange.huxtable <- function(.data, ...) {}
body(arrange.huxtable) <- body(arrange_.huxtable)


#' @importFrom dplyr transmute
#' @export
transmute.huxtable <- mutate.huxtable


#' @importFrom dplyr slice_
#' @export
slice_.huxtable <- function (.data, ..., .dots) {
  ht <- .data
  .data <- tibble::rownames_to_column(.data, 'slice.huxtable.rownames')
  result <- NextMethod()
  ht[na.omit(match(result$slice.huxtable.rownames, .data$slice.huxtable.rownames)), ]
}


#' @importFrom dplyr slice
#' @export
slice.huxtable <- function (.data, ...) {}
body(slice.huxtable) <- body(slice_.huxtable)


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


#' @importFrom dplyr select
#' @export
select.huxtable <- function(.data, ...) {}
body(select.huxtable) <- body(select_.huxtable)


#' @importFrom dplyr rename_
#' @export
rename_.huxtable <- select_.huxtable


#' @importFrom dplyr rename
#' @export
rename.huxtable <- select.huxtable
