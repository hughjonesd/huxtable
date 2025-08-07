#' Documentation helpers
#'
#' Internal utilities to keep documentation in sync with defaults.
#'
#' @param prop Property name in `huxtable_env$huxtable_default_attrs`.
#' @return A character string for inline Roxygen.
#' @noRd
.rd_default <- function(prop) {
  default <- huxtable_env$huxtable_default_attrs[[prop]]
  paste0("Set to `NA` to reset to the default, which is ", default, ".")
}
