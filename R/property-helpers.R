#' Property helper functions
#'
#' Internal helpers for getting and setting cell properties.
#'
#' @noRd
.prop_get <- function (ht, prop) {
  attr(ht, prop)
}

#' @noRd
.validate_prop <- function (value, prop, check_fun = NULL, check_values = NULL) {
  if (! all(is.na(value))) {
    if (! is.null(check_fun)) stopifnot(check_fun(value))
    if (! is.null(check_values)) {
      stopifnot(all(na.omit(value) %in% check_values))
    }
  }
  value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  value
}

#' @noRd
.prop_replace <- function (ht, value, prop, check_fun = NULL, check_values = NULL, extra = NULL) {
  value <- .validate_prop(value, prop, check_fun, check_values)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[] <- value
  mode(attr(ht, prop)) <- mode(value)
  ht
}

#' @noRd
.prop_set <- function (ht, row, col, value, prop, check_fun = NULL, check_values = NULL, extra = NULL) {
  assert_that(is_huxtable(ht))
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  rc <- list()
  rc$row <- get_rc_spec(ht, row, 1)
  rc$col <- get_rc_spec(ht, col, 2)
  value <- .validate_prop(value, prop, check_fun, check_values)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' @noRd
.prop_map <- function (ht, row, col, fn, prop, check_fun = NULL, check_values = NULL, extra = NULL) {
  assert_that(is_huxtable(ht))
  if (missing(col) && missing(fn)) {
    fn <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  rc <- list()
  rc$row <- get_rc_spec(ht, row, 1)
  rc$col <- get_rc_spec(ht, col, 2)
  current <- attr(ht, prop)[rc$row, rc$col, drop = FALSE]
  if (is_huxtable(current)) current <- as.matrix(current)
  value <- fn(ht, rc$row, rc$col, current)
  value <- .validate_prop(value, prop, check_fun, check_values)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}
