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

#' Assert condition unless all values are NA
#'
#' @param value Values to check
#' @param ... Conditions passed to [assertthat::assert_that]
#' @noRd
assert_not_all_na <- function(value, ...) {
  if (!all(is.na(value))) assert_that(...)
  invisible(TRUE)
}

#' Prepare row/col/fn arguments for map_* wrappers
#'
#' @noRd
prep_map_args <- function(ht, row, col, fn) {
  if (missing(col) && missing(fn)) {
    fn <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  list(row = row, col = col, fn = fn)
}

#' Prepare row/col/value arguments for set_* wrappers
#'
#' @noRd
prep_set_args <- function(ht, row, col, value) {
  if (missing(col) && missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
    col <- seq_len(ncol(ht))
  } else {
    if (missing(row)) row <- seq_len(nrow(ht))
    if (missing(col)) col <- seq_len(ncol(ht))
  }
  list(row = row, col = col, value = value)
}

#' Replace an entire property matrix/vector
#'
#' @param ht           A huxtable.
#' @param value        New property values.
#' @param prop         Property name.
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#' @param coerce_mode  If `TRUE`, coerce the stored matrix mode to match `value`.
#'
#' @noRd
prop_replace <- function(ht, value, prop, reset_na = TRUE,
                          coerce_mode = TRUE) {
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
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
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#'
#' @noRd
prop_set <- function(ht, row, col, value, prop,
                      reset_na = TRUE) {
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
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' Map a function over a cell-based property
#'
#' @param fn A mapping function. See [mapping-functions].
#' @inheritParams prop_set
#'
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#' @noRd
prop_map <- function(ht, row, col, fn, prop,
                      reset_na = TRUE) {
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
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  attr(ht, prop)[rc$row, rc$col] <- value
  ht
}

#' Set values for a row-based property
#'
#' @inheritParams prop_set
#'
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#' @noRd
prop_set_row <- function(ht, row, value, prop,
                          reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- row
    row <- seq_len(nrow(ht))
  }
  row <- get_rc_spec(ht, row, 1)
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  attr(ht, prop)[row] <- value
  ht
}

#' Set values for a column-based property
#'
#' @inheritParams prop_set
#'
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#' @noRd
prop_set_col <- function(ht, col, value, prop,
                          reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- col
    col <- seq_len(ncol(ht))
  }
  col <- get_rc_spec(ht, col, 2)
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  attr(ht, prop)[col] <- value
  ht
}

#' Set a table-level property
#'
#' @inheritParams prop_set
#'
#' @param reset_na     Should `NA` values be replaced with the huxtable default?
#' @noRd
prop_set_table <- function(ht, value, prop,
                            reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (reset_na) {
    value[is.na(value)] <- huxtable_env$huxtable_default_attrs[[prop]]
  }
  attr(ht, prop) <- value
  ht
}

