#' Property helper functions
#'
#' Internal helpers for getting and setting huxtable properties.
#'
#' @noRd
.prop_get <- function (ht, prop) {
  attr(ht, prop)
}

#' Validate and normalise property values
#'
#' @param value        Proposed property values.
#' @param prop         Property name.
#' @param check_fun    Optional validation function.
#' @param check_values Optional vector of allowed values.
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#'
#' @return Normalised `value`.
#' @noRd
.validate_prop <- function (value, prop, check_fun = NULL, check_values = NULL,
        reset_na = TRUE) {
  if (! all(is.na(value))) {
    if (! is.null(check_fun)) stopifnot(check_fun(value))
    if (! is.null(check_values)) {
      stopifnot(all(na.omit(value) %in% check_values))
    }
  }
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  value
}

#' Replace an entire property matrix/vector
#'
#' @param ht           A huxtable.
#' @param value        New property values.
#' @param prop         Property name.
#' @param check_fun    Optional validation function.
#' @param check_values Optional vector of allowed values.
#' @param extra        Extra code to run after validation.
#' @param reset_na     Passed to [`.validate_prop`].
#' @param coerce_mode  If `TRUE`, coerce the stored matrix mode to match `value`.
#'
#' @noRd
.prop_replace <- function (ht, value, prop, check_fun = NULL, check_values = NULL,
        extra = NULL, reset_na = TRUE, coerce_mode = TRUE) {
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[] <- value
  if (coerce_mode) mode(attr(ht, prop)) <- mode(value)
  ht
}

#' Set property values for a cell-based property
#'
#' @param ht           A huxtable.
#' @param row,col      Row/column specifiers.
#' @param value        Property values.
#' @param prop         Property name.
#' @param check_fun    Optional validation function.
#' @param check_values Optional vector of allowed values.
#' @param extra        Extra code to run after validation.
#' @param reset_na     Passed to [`.validate_prop`].
#'
#' @noRd
.prop_set <- function (ht, row, col, value, prop, check_fun = NULL,
        check_values = NULL, extra = NULL, reset_na = TRUE) {
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
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' Map a function over a cell-based property
#'
#' @param fn A mapping function. See [mapping-functions].
#' @inheritParams .prop_set
#'
#' @noRd
.prop_map <- function (ht, row, col, fn, prop, check_fun = NULL,
        check_values = NULL, extra = NULL, reset_na = TRUE) {
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
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' Set values for a row-based property
#'
#' @inheritParams .prop_set
#' @noRd
.prop_set_row <- function (ht, row, value, prop, check_fun = NULL,
        check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
  }
  row <- get_rc_spec(ht, row, 1)
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[row] <- value
  ht
}

#' Set values for a column-based property
#'
#' @inheritParams .prop_set
#' @noRd
.prop_set_col <- function (ht, col, value, prop, check_fun = NULL,
        check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- col
    col <- seq_len(ncol(ht))
  }
  col <- get_rc_spec(ht, col, 2)
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop)[col] <- value
  ht
}

#' Set a table-level property
#'
#' @inheritParams .prop_set
#' @noRd
.prop_set_table <- function (ht, value, prop, check_fun = NULL,
        check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  value <- .validate_prop(value, prop, check_fun, check_values, reset_na)
  if (! is.null(extra)) eval(extra)
  attr(ht, prop) <- value
  ht
}
