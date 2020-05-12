
#' Default huxtable properties
#'
#' Defaults are used for new huxtables, and also when a property is set to `NA`.
#'
#' @param ... Properties specified by name, or a single named list.
#'
#' @return For `set_default_properties`, a list of the previous property values, invisibly.
#' @details
#' Note that `autoformat = TRUE` in [huxtable()] overrides some defaults.
#' @export
#' @seealso Options for autoformat in [huxtable-options].
#' @examples
#' old <- set_default_properties(left_border = 1)
#' hux(a = 1:2, b = 1:2)
#' set_default_properties(old)
set_default_properties <- function(...) {
  defaults <- list(...)
  if (is.list(defaults[[1]]) && is.null(names(defaults))) defaults <- defaults[[1]]
  check_recognized_properties(names(defaults))

  old <- huxtable_env$huxtable_default_attrs[names(defaults)]
  huxtable_env$huxtable_default_attrs[names(defaults)] <- defaults

  invisible(old)
}


#' Get default huxtable properties
#'
#' @param names Vector of property names. If `NULL`, all properties are returned.
#'
#' @return For `get_default_properties`, a list of the current defaults.
#' @export
#'
#' @examples
#' get_default_properties("bold")
#' @rdname set_default_properties
get_default_properties <- function (names = NULL) {
  names <- names %||% names(huxtable_env$huxtable_default_attrs)
  check_recognized_properties(names)

  huxtable_env$huxtable_default_attrs[names]
}


check_recognized_properties <- function (names) {
  if (length(unrec <- setdiff(names, names(huxtable_env$huxtable_default_attrs))) > 0) stop(
    "Unrecognized property name(s): ", paste(unrec, collapse = ", "),
    "; to see all names, use get_default_properties()")
}
