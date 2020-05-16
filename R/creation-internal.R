

#' Create a new huxtable. Internal.
#'
#' @param x Object that can be coerced by as.data.frame
#'
#' @return A valid huxtable
#' @noRd
#'
new_huxtable <- function (x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  for (a in setdiff(huxtable_cell_attrs, "number_format")) {
    attr(x, a) <- matrix(NA, nrow(x), ncol(x))
  }
  attr(x, "number_format") <- matrix(list(NA), nrow(x), ncol(x))
  for (a in huxtable_col_attrs) {
    attr(x, a) <- rep(NA, ncol(x))
  }
  for (a in huxtable_row_attrs) {
    attr(x, a) <- rep(NA, nrow(x))
  }
  for (a in huxtable_table_attrs) {
    attr(x, a) <- NA
  }
  attr(x, "lr_borders") <- list(
    thickness = matrix(0, nrow(x), ncol(x) + 1),
    style     = matrix("solid", nrow(x), ncol(x) + 1),
    color     = matrix(NA_character_, nrow(x), ncol(x) + 1)
  )
  attr(x, "tb_borders") <- list(
    thickness = matrix(0, nrow(x) + 1, ncol(x)),
    style     = matrix("solid", nrow(x) + 1, ncol(x)),
    color     = matrix(NA_character_, nrow(x) + 1, ncol(x))
  )

  for (a in names(huxtable_env$huxtable_default_attrs)) {
    attr(x, a)[] <- huxtable_env$huxtable_default_attrs[[a]]  # [[ indexing matters here
  }

  x <- set_attr_dimnames(x)
  class(x) <- c("huxtable", class(x))

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

