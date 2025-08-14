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


#' Set property values or apply mapping function for a cell-based property
#'
#' @param ht           A huxtable.
#' @param row,col      Row/column specifiers. If both missing, sets entire property.
#' @param value        Property values (for setting).
#' @param fn           Mapping function (for mapping).
#' @param prop         Property name.
#' @param check_fun    Optional validation function.
#' @param check_values Optional vector of allowed values.
#' @param extra        Extra code to run after validation.
#' @param reset_na     Passed to [`validate_prop`].
#'
#' @noRd
prop_set <- function(ht, prop, row, col, value = NULL, fn = NULL,
                     check_fun = NULL, check_values = NULL, extra = NULL,
                     reset_na = TRUE) {
  assert_that(is_huxtable(ht))

  # Handle two-argument form: set_*(ht, value) or map_*(ht, fn)
  if (missing(col)) {
    if (missing(value) && missing(fn)) {
      if (is.function(row)) {
        fn <- row
      } else {
        value <- row
      }
      row <- seq_len(nrow(ht))
      col <- seq_len(ncol(ht))
    } else {
      # Standard missing col case
      col <- seq_len(ncol(ht))
    }
  }

  # Handle missing arguments for standard form
  if (missing(row)) row <- seq_len(nrow(ht))
  if (missing(col)) col <- seq_len(ncol(ht))

  rc <- list()
  rc$row <- get_rc_spec(ht, row, 1)
  rc$col <- get_rc_spec(ht, col, 2)

  # Compute value if we are mapping
  if (!is.null(fn)) {
    current <- attr(ht, prop)[rc$row, rc$col, drop = FALSE]
    if (is_huxtable(current)) current <- as.matrix(current)
    value <- fn(ht, rc$row, rc$col, current)
  }

  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  attr(ht, prop)[rc$row, rc$col] <- value

  # Coerce mode when setting entire property with simple values
  # But preserve list-matrix structure for properties that need it
  if (identical(rc$row, seq_len(nrow(ht))) &&
      identical(rc$col, seq_len(ncol(ht))) &&
      !is.list(attr(ht, prop))) {
    mode(attr(ht, prop)) <- mode(value)
  }
  ht
}


#' Set values for a dimension-based property (internal helper)
#'
#' @inheritParams prop_set
#' @param dimension 1 for rows, 2 for columns
#' @param dim_spec Row or column specification
#' @noRd
prop_set_dim <- function(ht, dim_spec, value, prop, dimension, check_fun = NULL,
                         check_values = NULL, extra = NULL, reset_na = TRUE) {
  assert_that(is_huxtable(ht))
  if (missing(value)) {
    value <- dim_spec
    dim_spec <- if (dimension == 1) seq_len(nrow(ht)) else seq_len(ncol(ht))
  }
  dim_spec <- get_rc_spec(ht, dim_spec, dimension)
  value <- validate_prop(value, prop, check_fun, check_values, reset_na)
  if (!is.null(extra)) eval(extra)
  
  attr(ht, prop)[dim_spec] <- value
  
  # Coerce mode if setting entire property
  size <- if (dimension == 1) nrow(ht) else ncol(ht)
  if (identical(dim_spec, seq_len(size))) {
    mode(attr(ht, prop)) <- mode(value)
  }
  ht
}

#' Set values for a row-based property
#'
#' @inheritParams prop_set
#' @noRd
prop_set_row <- function(ht, row, value, prop, check_fun = NULL,
                         check_values = NULL, extra = NULL, reset_na = TRUE) {
  prop_set_dim(ht, row, value, prop, dimension = 1, check_fun = check_fun,
               check_values = check_values, extra = extra, reset_na = reset_na)
}

#' Set values for a column-based property
#'
#' @inheritParams prop_set
#' @noRd
prop_set_col <- function(ht, col, value, prop, check_fun = NULL,
                         check_values = NULL, extra = NULL, reset_na = TRUE) {
  prop_set_dim(ht, col, value, prop, dimension = 2, check_fun = check_fun,
               check_values = check_values, extra = extra, reset_na = reset_na)
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
