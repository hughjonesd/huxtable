#' Documentation helpers
#'
#' Internal utilities to keep documentation in sync with defaults.
#'
#' @param prop Property name in `huxtable_env$huxtable_default_attrs`.
#' @return A character string for inline Roxygen.
#' @noRd
rd_default <- function(prop) {
  default <- huxtable_env$huxtable_default_attrs[[prop]]
  default <- deparse(default)
  paste0("Set to `NA` to reset to the default, which is `", default, "`.")
}

# Shared documentation for huxtable property functions:

#' @param ht A huxtable.
#' @param row A row specifier. See [rowspecs] for details.
#' @param col An optional column specifier.
#' @param fn A mapping function. See [mapping-functions] for details.
#' @return
#'   `property()` returns the property value(s).
#'   `set_property()` and `map_property()` return the modified huxtable.
#'
#' @name hux_prop_params
NULL
