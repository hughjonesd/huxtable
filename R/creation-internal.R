#' Create a new huxtable. Internal.
#'
#' @param x Object that can be coerced by as.data.frame
#'
#' @return A valid huxtable
#' @noRd
#'
new_huxtable <- function(x) {
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
    thickness = matrix(huxtable_env$huxtable_default_attrs[["border"]], nrow(x), ncol(x) + 1),
    style     = matrix(huxtable_env$huxtable_default_attrs[["border_style"]], nrow(x), ncol(x) + 1),
    color     = matrix(huxtable_env$huxtable_default_attrs[["border_color"]], nrow(x), ncol(x) + 1)
  )
  attr(x, "tb_borders") <- list(
    thickness = matrix(huxtable_env$huxtable_default_attrs[["border"]], nrow(x) + 1, ncol(x)),
    style     = matrix(huxtable_env$huxtable_default_attrs[["border_style"]], nrow(x) + 1, ncol(x)),
    color     = matrix(huxtable_env$huxtable_default_attrs[["border_color"]], nrow(x) + 1, ncol(x))
  )

  non_border_attrs <- grep("border",
    names(huxtable_env$huxtable_default_attrs),
    value = TRUE, invert = TRUE
  )
  for (a in non_border_attrs) {
    attr(x, a)[] <- huxtable_env$huxtable_default_attrs[[a]] # [[ indexing matters here
  }

  x <- set_attr_dimnames(x)
  class(x) <- c("huxtable", class(x))

  x
}
