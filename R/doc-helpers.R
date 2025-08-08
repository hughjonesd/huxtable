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

#' Shared parameter documentation for huxtable property helpers
#'
#' Use this with `@inheritParams` to document common property helper
#' arguments.
#'
#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param fn A mapping function. See [mapping-functions] for details.
#' @keywords internal
#'
#' @name .hux_prop_params
NULL
