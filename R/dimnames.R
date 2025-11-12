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
#'   First = rnorm(5),
#'   Second = rnorm(5),
#'   add_rownames = FALSE
#' )
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
add_colnames <- function(ht, rowname = "", ...) {
  if (!missing(rowname)) assert_that(is.null(rowname) || is.string(rowname))

  dateish_cols <- which(sapply(ht, function(x) class(x)[1] %in% c("Date", "POSIXct", "POSIXlt")))
  for (col in dateish_cols) ht[[col]] <- as.character(ht[[col]]) # avoids autoconversion by c(),
  # which uses as.numeric

  cn <- colnames(ht)
  rn <- rownames(ht)
  cn_hux <- new_huxtable(matrix(cn, 1, length(cn)))
  ht <- rbind(cn_hux, ht, copy_cell_props = FALSE)
  number_format(ht)[1, ] <- NA
  colnames(ht) <- cn
  header_rows(ht)[1] <- TRUE
  if (!is.null(rowname)) rownames(ht) <- c(rowname, rn)

  ht
}


#' @export
#' @rdname add_colnames
add_rownames <- function(ht, colname = "rownames", preserve_rownames = TRUE, ...) {
  assert_that(is.string(colname))
  ht <- cbind(rownames(ht), ht, copy_cell_props = FALSE)
  number_format(ht)[, 1] <- NA
  header_cols(ht)[1] <- TRUE
  colnames(ht)[1] <- colname
  if (!preserve_rownames) rownames(ht) <- NULL

  ht
}


#' @export
`dimnames<-.huxtable` <- function(x, value) {
  x <- NextMethod()
  x <- set_attr_dimnames(x)

  x
}


#' @export
`names<-.huxtable` <- function(x, value) {
  x <- NextMethod()
  x <- set_attr_dimnames(x)

  x
}


#' @export
`row.names<-.huxtable` <- function(x, value) {
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
