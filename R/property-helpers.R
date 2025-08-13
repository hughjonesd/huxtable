#' @import assertthat
#' @importFrom stats na.omit
#' @evalNamespace make_exports(huxtable_col_attrs)
#' @evalNamespace make_exports(huxtable_row_attrs)
#' @evalNamespace make_exports(huxtable_table_attrs)
#' @evalNamespace make_exports(huxtable_cell_attrs, with_map = TRUE)
NULL

huxtable_cell_attrs <- c(
  "align",
  "valign",
  "rowspan",
  "colspan",
  "background_color",
  "text_color",
  "top_padding",
  "left_padding",
  "right_padding",
  "bottom_padding",
  "wrap",
  "markdown",
  "escape_contents",
  "na_string",
  "bold",
  "italic",
  "font_size",
  "rotation",
  "number_format",
  "font"
)
huxtable_col_attrs <- c("col_width", "header_cols")
huxtable_row_attrs <- c("row_height", "header_rows")
huxtable_table_attrs <- c(
  "width",
  "height",
  "position",
  "caption",
  "caption_pos",
  "caption_width",
  "tabular_environment",
  "table_environment",
  "label",
  "latex_float"
)

huxtable_env <- new.env()
huxtable_env$huxtable_default_attrs <- list(
  rowspan             = 1,
  colspan             = 1,
  align               = "left",
  valign              = "top",
  width               = NA_real_,
  height              = NA_real_,
  col_width           = NA_real_,
  row_height          = NA_real_,
  header_cols         = FALSE,
  header_rows         = FALSE,
  background_color    = NA_character_,
  text_color          = NA_character_,
  border              = 0,
  border_color        = NA_character_,
  border_style        = "solid",
  left_padding        = 6,
  right_padding       = 6,
  top_padding         = 6,
  bottom_padding      = 6,
  wrap                = TRUE,
  caption             = NA_character_,
  caption_pos         = "top",
  caption_width       = NA_real_,
  position            = "center",
  tabular_environment = NA_character_,
  table_environment   = "table",
  label               = NA_character_,
  latex_float         = "ht",
  markdown            = FALSE,
  escape_contents     = TRUE,
  na_string           = "",
  bold                = FALSE,
  italic              = FALSE,
  font_size           = NA_real_,
  rotation            = 0,
  number_format       = list("%.3g"),
  font                = NA_character_
)

#' Property helper functions
#'
#' Internal helpers for getting and setting huxtable properties.
#'
#' @noRd
prop_get <- function(ht, prop) {
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
validate_prop <- function(value, prop, check_fun = NULL, check_values = NULL,
                          reset_na = TRUE) {
  if (!all(is.na(value))) {
    if (!is.null(check_fun)) stopifnot(check_fun(value))
    if (!is.null(check_values)) {
      stopifnot(all(na.omit(value) %in% check_values))
    }
  }
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  value
}

#' Extract row/column arguments and handle missingness
#'
#' @param ht A huxtable.
#' @param row Row specifier (can be missing).
#' @param col Column specifier (can be missing).
#' @param value_or_fn Value or function (used when row/col are missing).
#'
#' @return List with row, col, and value_or_fn components.
#' @noRd
parse_rc_args <- function(ht, row, col, value_or_fn) {
  if (missing(row) && missing(col)) {
    # Setting entire property: row and col both missing
    list(
      row = seq_len(nrow(ht)),
      col = seq_len(ncol(ht)),
      value_or_fn = value_or_fn
    )
  } else if (missing(col) && missing(value_or_fn)) {
    # Two argument form: prop_set(ht, value)
    list(
      row = seq_len(nrow(ht)),
      col = seq_len(ncol(ht)),
      value_or_fn = row
    )
  } else {
    # Standard form
    list(
      row = if (missing(row)) seq_len(nrow(ht)) else row,
      col = if (missing(col)) seq_len(ncol(ht)) else col,
      value_or_fn = value_or_fn
    )
  }
}

#' Set property values for a cell-based property
#'
#' @param ht           A huxtable.
#' @param row,col      Row/column specifiers. If both missing, sets entire property.
#' @param value        Property values.
#' @param prop         Property name.
#' @param check_fun    Optional validation function.
#' @param check_values Optional vector of allowed values.
#' @param extra        Extra code to run after validation.
#' @param reset_na     Passed to [`validate_prop`].
#' @param coerce_mode  If `TRUE`, coerce the stored matrix mode to match `value`.
#'
#' @noRd
prop_set <- function(ht, row, col, value, prop, check_fun = NULL,
                     check_values = NULL, extra = NULL, reset_na = TRUE,
                     coerce_mode = TRUE) {
  assert_that(is_huxtable(ht))
  
  # Parse arguments and handle missingness
  parsed <- parse_rc_args(ht, row, col, value)
  rc <- list()
  rc$row <- get_rc_spec(ht, parsed$row, 1)
  rc$col <- get_rc_spec(ht, parsed$col, 2)
  value <- validate_prop(parsed$value_or_fn, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  
  # Coerce mode if setting entire property
  if (coerce_mode && identical(rc$row, seq_len(nrow(ht))) && identical(rc$col, seq_len(ncol(ht)))) {
    mode(attr(ht, prop)) <- mode(value)
  }
  ht
}

#' Map a function over a cell-based property
#'
#' @param fn A mapping function. See [mapping-functions].
#' @inheritParams prop_set
#'
#' @noRd
prop_map <- function(ht, row, col, fn, prop, check_fun = NULL,
                     check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  
  # Parse arguments and handle missingness
  parsed <- parse_rc_args(ht, row, col, fn)
  rc <- list()
  rc$row <- get_rc_spec(ht, parsed$row, 1)
  rc$col <- get_rc_spec(ht, parsed$col, 2)
  current <- attr(ht, prop)[rc$row, rc$col, drop = FALSE]
  if (is_huxtable(current)) current <- as.matrix(current)
  value <- parsed$value_or_fn(ht, rc$row, rc$col, current)
  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' Set values for a row-based property
#'
#' @inheritParams prop_set
#' @noRd
prop_set_row <- function(ht, row, value, prop, check_fun = NULL,
                         check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
  }
  row <- get_rc_spec(ht, row, 1)
  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop)[row] <- value
  ht
}

#' Set values for a column-based property
#'
#' @inheritParams prop_set
#' @noRd
prop_set_col <- function(ht, col, value, prop, check_fun = NULL,
                         check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- col
    col <- seq_len(ncol(ht))
  }
  col <- get_rc_spec(ht, col, 2)
  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop)[col] <- value
  ht
}

#' Set a table-level property
#'
#' @inheritParams prop_set
#' @noRd
prop_set_table <- function(ht, value, prop, check_fun = NULL,
                           check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop) <- value
  ht
}
