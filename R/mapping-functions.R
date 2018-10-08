
#' @import assertthat
NULL


#' How to set cell properties variably by cell contents
#'
#' This help page explains how to set properties differently for cells, depending on their contents.
#'
#' For example, in a table of p-values, you could bold cells where p < 0.05:
#'
#' ```
#'   map_bold(pval_hux, by_ranges(0.05, c(TRUE, FALSE)))
#' ```
#'
#'  Or you can use red text for a particular value:
#'
#' ```
#'   hxtbl %>% map_text_color(by_values("Warning" = "red"))
#' ```
#'
#' There is a `map_xxx` function for each huxtable cell property. The syntax is:
#'
#' ```
#'   map_xxx(ht, row, col, fn)
#' ```
#'
#' where `xxx` is the property name.
#'
#' `row` and `col` specify ranges of rows and columns. See [rowspecs] for details. To set properties
#' for the whole table, you can omit `row` and `col`:
#'
#' ```
#'   map_xxx(ht, fn)
#' ```
#'
#' The `fn` argument is a *mapping function* which maps cell contents to property values.
#'
#' * To set property values in "stripes" by rows or by columns, use [by_rows()] and [by_cols()].
#' * To set property values for cells with specific contents, use [by_values()].
#' * To set property values for cells within a numeric range, use [by_ranges()].
#' * To set property values for cells by quantiles, use [by_quantiles()] or [by_equal_groups()].
#' * To set property values for cells that match a string or regular expression, use [by_regex()].
#' * To map numeric values to a colorspace, use [by_colorspace()].
#' * For a more general solution, use [by_function()] or [by_cases()].
#'
#' @section Technical details:
#'
#' `fn` must be a function taking four arguments: the (entire) original huxtable `ht`, a numeric
#' vector of `rows`, a numeric vector of `cols`, and the `current` property values for `ht[rows,
#' cols]`, as a matrix. It should return the new property values for `ht[rows, cols]`, as a matrix.
#'
#' Here's an example. Suppose we want to highlight cells
#' of a correlation matrix with low p values:
#'
#' ```
#'   data(attitudes)
#'   att_corr <- psych::corr.test(attitudes)
#'   # att_corr has components r (correlations) and p (p values)
#'
#'   corr_hux <- as_hux(att_corr$r)
#'   by_p_value <- function (ht, rows, cols, current) {
#'      result <- current
#'      pvals <- att_corr$p[rows, cols]
#'      result[pvals < 0.01] <- "red"
#'      result[pvals < 0.05] <- "orange"
#'      result
#'   }
#   map_background_color(corr_hux, by_p_value)
#' ```
#'
#'
#' @return The modified huxtable.
#'
#' @examples
#' ht <- hux(c("OK", "Warning", "Error"))
#' ht <- map_text_color(ht, by_values(
#'         OK      = "green",
#'         Warning = "orange",
#'         Error   = "red"
#'       ))
#' ht
#'
#' ht <- hux(rnorm(5), rnorm(5), rnorm(5))
#' map_background_color(ht, by_ranges(
#'         c(-1, 1),
#'         c("blue", "yellow", "red")
#'       ))
#' map_background_color(ht,
#'       by_equal_groups(2, c("red", "green")))
#'
#' ht <- hux(
#'         Coef = c(3.5, 2.4, 1.3),
#'         Pval = c(0.04, 0.01, 0.07),
#'         add_colnames = TRUE
#'       )
#' map_bold(ht, everywhere, "Pval",
#'       by_ranges(0.05, c(TRUE, FALSE)))
#' @name mapping-functions
#' @aliases mapping_functions
NULL


#' Map specific cell values to cell properties
#'
#' @param ... Name-value pairs like `name = value`. Cells where contents are equal to
#'   `name` will have the property set to `value`. If there is a single unnamed argument,
#'   this is the default value for unmatched cells. More than one unnamed argument is an error.
#'
#' @return A function for use in `map_***` functions.
#'
#' @family mapping functions
#' @seealso [mapping-functions]
#'
#' @export
#'
#' @examples
#' ht <- hux(letters[1:3])
#' map_background_color(ht,
#'       by_values(a = "red", c = "yellow"))
#' map_background_color(ht,
#'       by_values(a = "red", c = "yellow", "green"))
by_values <- function (...) {
  vals <- c(...)
  named_vals <- vals[names(vals) != ""]
  targets <- names(named_vals)
  default <- vals[names(vals) == ""]
  if (length(default) > 1) stop("At most one element of `...` can be unnamed")

  values_fn <- function (ht, rows, cols, current) {
    res <- current
    if (length(default) > 0) res[] <- default
    for (tg in targets) {
      res[ ht[rows, cols] == tg ] <- named_vals[[tg]]
    }
    res
  }

  return(values_fn)
}


#' Set cell properties by row or column
#'
#' `by_rows` and `by_cols` set properties in horizontal or vertical "stripes".
#'
#' @param ... One or more cell property values.
#' @param from Numeric. Row or column to start at.
#'
#' @inherit by_values return
#' @family mapping functions
#' @seealso [mapping-functions]
#' @export
#'
#' @examples
#' ht <- as_hux(matrix(rnorm(25), 5, 5))
#' map_background_color(ht,
#'       by_rows("green", "grey"))
#' map_background_color(ht,
#'       by_cols("green", "grey"))
by_rows <- function (..., from = 1) {
  vals <- c(...)
  assert_that(is.count(from))

  row_fn <- function (ht, rows, cols, current) {
    res <- current
    assert_that(from <= nrow(res))
    res[seq(from, nrow(res)), ] <- rep(vals, length.out = nrow(res) - from + 1)
    res
  }

  return(row_fn)
}


#' @export
#' @rdname by_rows
by_cols <- function (..., from = 1) {
  vals <- c(...)

  col_fn <- function (ht, rows, cols, current) {
    res <- current
    assert_that(from <= ncol(res))
    lout <- ncol(res) - from + 1
    vals <- matrix(rep(vals, length.out = lout), ncol = lout, nrow = nrow(res), byrow = TRUE)
    res[, seq(from, ncol(res))] <- vals
    res
  }

  return(col_fn)
}


#' Map numeric ranges to cell properties
#'
#' `by_ranges` sets property values for cells falling within different numeric ranges.
#'
#' @param breaks A vector of numbers in increasing order.
#' @param values A vector of property values. `length(values)` should be one greater than
#'   `length(breaks)` if `extend = TRUE`, or one less if `extend = FALSE`.
#' @param right Intervals are closed on the right. I.e. if values are exactly equal to a `break`,
#'   put them in the lower group.
#' @param extend Extend `breaks` to `c(-Inf, breaks, Inf)`, i.e. include numbers outside the range.
#'  `TRUE` by default.
#'
#' @details
#' Non-numeric cells are unchanged.
#'
#' @inherit by_values return
#' @family mapping functions
#' @seealso [mapping-functions]
#'
#' @export
#'
#' @examples
#' ht <- huxtable(c(1, 3, 5))
#' map_background_color(ht,
#'       by_ranges(
#'         c(2, 4),
#'         c("red", "yellow", "blue")
#'       ))
#'
#' map_background_color(ht,
#'       by_ranges(
#'         c(2, 4),
#'         "pink",
#'         extend = FALSE
#'       ))
#'
#' map_background_color(ht,
#'       by_ranges(
#'         c(1, 5),
#'         c("red", "yellow", "green"),
#'         right = TRUE
#'       ))
#' map_background_color(ht,
#'       by_ranges(
#'         c(1, 5),
#'         c("red", "yellow", "green"),
#'         right = FALSE
#'       ))
by_ranges <- function (breaks, values, right = FALSE, extend = TRUE) {
  assert_that(is.numeric(breaks))
  assert_that(all(breaks == sort(breaks)))
  if (extend) breaks <- c(-Inf, breaks, Inf)
  assert_that(length(values) == length(breaks) - 1, msg = "`values` is wrong length")
  force(right)

  ranges_fn <- function(ht, rows, cols, current) {
    res <- current
    mx <- as.matrix(ht)[rows, cols]
    # avoid "NAs introduced by coercion"
    which_val <- suppressWarnings(findInterval(mx, breaks, left.open = right))
    which_val[is.na(which_val)] <- 0
    which_val[which_val == length(breaks)] <- 0
    res[which_val > 0] <- values[which_val[which_val > 0]]
    res
  }

  ranges_fn
}


#' Map numeric quantiles to cell properties
#'
#' These functions split cell values by quantiles. Non-numeric cells are ignored.
#'
#' @inheritParams by_ranges
#' @param quantiles Vector of quantiles.
#' @param values Vector of values. `length(values)` should be one greater than `length(quantiles)`,
#'   or one less if `extend = FALSE`.
#'
#' @details
#' `by_equal_groups(n, values)` is a shortcut for `by_quantiles(seq(1/n, 1 - 1/n, 1/n), values)`.
#' @family mapping functions
#' @seealso [mapping-functions]
#' @inherit by_values return
#' @export
#'
#' @examples
#' ht <- hux(rnorm(5), rnorm(5))
#'
#' map_background_color(ht,
#'       by_quantiles(
#'         c(0.2, 0.8),
#'         c("red", "white", "green")
#'       ))
#'
#' map_background_color(ht,
#'       by_equal_groups(
#'         3,
#'         c("red", "yellow", "green")
#'       ))
by_quantiles <- function (quantiles, values, right = FALSE, extend = TRUE) {
  assert_that(is.numeric(quantiles), all(quantiles <= 1), all(quantiles >= 0))
  assert_that(all(quantiles == sort(quantiles)))
  force(extend)
  force(right)

  qr_fn <- function (ht, rows, cols, current) {
    vals <- as.matrix(ht)[rows, cols]
    vals <- suppressWarnings(as.numeric(vals))
    q_breaks <- stats::quantile(vals, quantiles, na.rm = TRUE, names = FALSE)
    rf <- by_ranges(q_breaks, values, right = right, extend = extend)
    rf(ht, rows, cols, current)
  }

  qr_fn
}


#' @param n Number of equal-sized groups. `length(values)` should equal `n`.
#'
#' @rdname by_quantiles
#' @export
by_equal_groups <- function (n, values) {
  by_quantiles(seq(1/n, 1 - 1/n, 1/n), values)
}


#' Map cells matching a string or regex to cell properties
#'
#' @param ... A list of name-value pairs. The names are regular expressions. If there is a single
#'   unnamed argument, this is the default value for unmatched cells. More than one unnamed argument
#'   is an error.
#' @param .grepl_args A list of arguments to pass to [grepl()]. Useful options
#'   include `fixed`, `perl` and `ignore.case`.
#'
#' @family mapping functions
#' @seealso [mapping-functions]
#' @inherit by_values return
#' @export
#'
#' @examples
#' ht <- hux("The cat sat", "on the", "mat")
#'
#' map_bold(ht, by_regex("at" = TRUE))
#' map_bold(ht, by_regex("a.*a" = TRUE))
#'
#' map_bold(ht, by_regex(
#'         "the" = TRUE,
#'         .grepl_args = list(
#'           ignore.case = TRUE
#'         )
#'       ))
by_regex <- function(..., .grepl_args = list()) {
  vals <- c(...)
  named_vals <- vals[names(vals) != ""]
  patterns <- names(named_vals)
  default <- vals[names(vals) == ""]
  if (length(default) > 1) stop("At most one element of `...` can be unnamed")

  matching_fn <- function (ht, rows, cols, current) {
    res <- current
    if (length(default) > 0) res[] <- default
    my_args <- .grepl_args
    my_args$x <- as.matrix(ht)[rows, cols]
    for (pt in patterns) {
      my_args$pattern <- pt
      matches <- do.call(grepl, my_args)
      res[matches] <- named_vals[[pt]]
    }
    res
  }

  return(matching_fn)
}


#' Map numeric cell contents smoothly to colors
#'
#' @param ... Colors
#' @param range Numeric endpoints. If `NULL`, these are determined from the data.
#' @param na_color Color to return for `NA` values. Use `NA` to set to the default.
#'
#' @details
#' `by_colorspace` requires the "scales" package.
#'
#' @family mapping functions
#' @seealso [mapping-functions]
#' @inherit by_values return
#' @export
#'
#' @examples
#'
#' if (! requireNamespace("scales")) {
#'   stop("Please install the scales package to run this example")
#' }
#' ht <- as_hux(matrix(rnorm(25), 5, 5))
#' map_background_color(ht,
#'       by_colorspace("red", "yellow", "blue"))
#'
by_colorspace <- function (..., range = NULL, na_color = NA) {
  assert_package("by_colorspace", "scales")
  palette <- c(...)

  by_function(scales::col_numeric(palette, domain = range, na.color = na_color))
}


#' Map cell contents to cell properties using a function or scale
#'
#' This creates a simple wrapper around a function for use in `map_xxx`.
#' Useful functions include scales and palettes from the `scales` package.
#'
#' @param inner_fn A one-argument function which maps cell values to property values.
#'
#' @details
#' The argument of `inner_fn` will be `as.matrix(ht[row, col])`. Be aware how matrix conversion
#' affects the `mode` of cell data.
#' @family mapping functions
#' @seealso [mapping-functions]
#' @inherit by_values return
#' @export
#'
#' @examples
#' ht <- as_hux(matrix(runif(20), 5, 4))
#'
#' map_background_color(ht,
#'       by_function(grey))
#'
#' if (requireNamespace("scales")) {
#'   map_text_color(ht, by_function(
#'           scales::seq_gradient_pal()
#'         ))
#' }
by_function <- function (inner_fn) {
  assert_that(is.function(inner_fn))
  wrapper_fn <- function (ht, rows, cols, current) {
    res <- current
    res[] <- inner_fn(as.matrix(ht[rows, cols]))
    res
  }

  return(wrapper_fn)
}


#' Map cell contents to properties using `case_when`
#'
#' This function uses [dplyr::case_when()] to set cell properties.
#'
#' @param ... A list of two-sided formulas interpreted by `case_when`.
#' @param skip_na Where `case_when` returns `NA`, leave the value unchanged. Otherwise, `NA` will
#'   normally reset the property value to the default.
#'
#' @details
#' Within the formulas, the variable `.` will refer to the content of `ht[rows, cols]` (converted
#' by `as.matrix`).
#'
#' `case_when` returns `NA` when no formula LHS is matched. By default this leaves the old property
#' value unchanged. To avoid this, set a default in the last formula: `TRUE ~ default`.
#'
#' @family mapping functions
#' @seealso [mapping-functions]
#' @inherit by_values return
#' @export
#'
#' @examples
#' if (! requireNamespace("dplyr")) {
#'   stop("Please install the 'dplyr' package to run this example")
#' }
#'
#' ht <- hux(runif(5), letters[1:5])
#'
#' map_background_color(ht, by_cases(
#'         . == "a" ~ "red",
#'         . %in% letters ~ "green",
#'         . < 0.5 ~ "pink"
#'       ))
by_cases <- function (..., skip_na = TRUE) {
  assert_that(is.flag(skip_na))
  assert_package("by_cases", "dplyr")
  # turn into character strings so they don't capture local information yet
  cases <- lapply(list(...), function (fml) Reduce(paste, deparse(fml)))

  case_fn <- function (ht, rows, cols, current) {
    res <- current
    myenv <- new.env()
    assign(".",  as.matrix(ht[rows, cols]), envir = myenv)
    cases <- lapply(cases, stats::as.formula, env = myenv)
    vals <- dplyr::case_when(!!! cases)
    res[] <- vals
    if (skip_na) res[is.na(vals)] <- current[is.na(vals)]
    res
  }

  return(case_fn)
}
