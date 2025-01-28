

#' @importFrom stats na.omit
NULL


filter.huxtable <- function(.data, ...) {
  ht <- .data
  .data <- as.data.frame(.data)
  .data$filter.huxtable.rownames <- rownames(.data)
  result <- NextMethod()
  ht[.data$filter.huxtable.rownames %in% result$filter.huxtable.rownames, ]
}


#' Use dplyr verbs with huxtable objects
#'
#' Huxtable can be used with dplyr verbs [dplyr::select()], [dplyr::rename()],
#' `dplyr::relocate()`, [dplyr::slice()], [dplyr::arrange()], [dplyr::mutate()]
#' and [`dplyr::transmute()`][dplyr::mutate]. These will return huxtables. Other verbs like
#' [dplyr::summarise()] will simply return data frames as normal;
#' [dplyr::pull()] will return a vector. `mutate` has an extra option, detailed
#' below.
#'
#' @param .data A huxtable.
#' @param ... Arguments passed to [dplyr::mutate()].
#' @param copy_cell_props Logical: copy cell and column properties from existing
#'   columns.
#'
#' @details If `mutate` creates new columns, and the argument `copy_cell_props`
#' is missing or `TRUE`, then cell and column properties will be copied from
#' existing columns to their left, if there are any. Otherwise, they will be the
#' standard defaults. Row and table properties, and properties of cells in
#' existing columns, remain unchanged.
#'
#' @rdname dplyr-verbs
#' @aliases mutate dplyr-verbs
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, c = 1:5, d = 1:5, add_colnames = FALSE)
#' bold(ht)[c(1, 3), ] <- TRUE
#' bold(ht)[, 1] <- TRUE
#' ht2 <- dplyr::select(ht, b:c)
#' ht2
#' bold(ht2)
#' ht3 <- dplyr::mutate(ht, x = a + b)
#' ht3
#' bold(ht3)
#' ht4 <- dplyr::mutate(ht, x = a + b,
#'       copy_cell_props = FALSE)
#' bold(ht4)
mutate.huxtable <- function (.data, ..., copy_cell_props = TRUE) {
  ht <- .data
  .data <- as.data.frame(.data)

  result <- switch(.Generic,
          "mutate"    = dplyr::mutate(.data, ...),
          "transmute" = dplyr::transmute(.data, ...),
          stop("Unrecognized function ", .Generic)
        )
  result <- as_hux(result, autoformat = FALSE, add_colnames = FALSE)

  for (a in c(huxtable_row_attrs, huxtable_table_attrs)) attr(result, a) <- attr(ht, a)

  match_cols <- match(colnames(result), colnames(ht))
  if (copy_cell_props) match_cols <- Reduce(function (x, y) if (is.na(y)) x else y, match_cols, accumulate = TRUE)
  result_cols <- ! is.na(match_cols)
  match_cols  <- match_cols[result_cols]

  for (a in huxtable_cell_attrs) attr(result, a)[, result_cols] <- attr(ht, a)[, match_cols]
  for (a in huxtable_col_attrs)  attr(result, a)[result_cols]  <- attr(ht, a)[match_cols]

  result <- set_attr_dimnames(result)

  result
}


transmute.huxtable <- mutate.huxtable


arrange.huxtable <- function(.data, ...) {
  ht <- .data
  .data <- as.data.frame(.data)
  .data$arrange.huxtable.rownames <- rownames(.data)
  result <- NextMethod()
  ht[match(result$arrange.huxtable.rownames, .data$arrange.huxtable.rownames), ]
}


slice.huxtable <- function (.data, ...) {
  ht <- .data
  .data <- as.data.frame(.data)
  .data$slice.huxtable.rownames <- rownames(.data)
  result <- NextMethod()
  ht[na.omit(match(result$slice.huxtable.rownames, .data$slice.huxtable.rownames)), ]

}


# The following functions will only be registered with dplyr if
# packageVersion("dplyr") <= "0.8.5".
# After that, we can just use the dplyr builtins. (Until they break
# subclasses again....)


select.huxtable <- function(.data, ...) {
  ht <- .data
  .data <- as.data.frame(t(colnames(.data)), stringsAsFactors = FALSE)
  colnames(.data) <- colnames(ht)
  result <- NextMethod()
  ht <- ht[, na.omit(match(result[1, ], colnames(ht)))]
  colnames(ht) <- colnames(result)

  ht
}


rename.huxtable <- select.huxtable
