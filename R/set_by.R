

#' Set properties by huxtable values
#'
#' This help page explains how to set properties according to the values in your table.
#'
#' For example, in a table of p-values, you could bold cells where p < 0.05:
#'
#' ```
#' pval_hux %>% set_bold_by(by_range(0.05, c(TRUE, FALSE)))
#' ```
#'
#'  Or you can use red text for a particular value:
#'
#' ```
#' hxtbl %>% set_text_color_by(by_value("Warning" = "red"))
#' ```
#'
#' There is a `set_xxx_by` function for each huxtable cell property. The syntax is:
#'
#' ```
#' set_xxx_by(ht, row, col, fn)
#' ```
#'
#' Here, `row` and `col` specify a range of rows and columns. See [rowspecs] for details.
#'
#' To set properties for the whole table, you can omit `row` and `col`:
#'
#' ```
#' set_xxx_by(ht, fn)
#' ```
#'
#' Use the `fn` argument to set properties in different ways.
#'
#' * To set a property for cells with a specific value, use [by_value()].
#' * To set a property for cells in a numeric range, use [by_range()].
#' * To set a property for cells by quantiles, use [by_quantile()] or [by_equal_size()].
#'
#' @section Technical details:
#'
#' The `fn` argument to `set_xxx_by` must be a function that takes four arguments: the
#' original huxtable `ht`, a numeric vector of `rows`, a numeric vector of `cols`, and the `current`
#' property values for `ht[rows, cols]`. It should return the new property values. Run e.g.
#' `by_value(1:3, letters[1:3])` to see an example.
#'
#' @name set-by
#'
#' @examples
#'
#' ht <- hux(c("OK", "Warning", "Error"))
#' ht <- set_text_color_by(ht, by_value(OK = "green", Warning = "orange", Error = "red"))
#' ht
#'
#' ht <- hux(rnorm(5), rnorm(5), rnorm(5))
#' set_background_color_by(ht, by_range(c(-1, 1), c("blue", "yellow", "red")))
#' set_background_color_by(ht, by_equal_size(2, c("red", "green")))
#'
#' ht <- hux(Coef = c(3.5, 2.4, 1.3), Pval = c(0.04, 0.01, 0.07), add_colnames = TRUE)
#' set_bold_by(ht, everywhere, "Pval", by_range(0.05, c(TRUE, FALSE)))
NULL



#' Set properties by specific values
#'
#' @param ... Name-value pairs like `name = value`. Cells where contents are equal to
#'   `name` will have the property set to `value`. If there is a single unnamed argument,
#'   this is the default value for unmatched cells. More than one unnamed argument is an error.
#'
#' @return A function for use in `set_***_by` functions.
#'
#' @family `by` functions
#' @seealso [set-by]
#'
#' @export
#'
#' @examples
#' ht <- hux(letters[1:3])
#' set_background_color_by(ht, by_value(a = "red", c = "yellow"))
#' set_background_color_by(ht, by_value(a = "red", c = "yellow", "green"))
by_value <- function (...) {
  vals <- c(...)
  named_vals <- vals[names(vals) != '']
  targets <- names(named_vals)
  default <- vals[names(vals) == '']
  if (length(default) > 1) stop('At most one element of `...` can be unnamed')

  values_fun <- function (ht, rows, cols, current) {
    res <- current
    if (length(default) > 0) res[] <- default
    for (tg in targets) {
      res[ ht[rows, cols] == tg ] <- named_vals[[tg]]
    }
    res
  }

  return(values_fun)
}


#' Set properties by numeric ranges
#'
#' `by_range` sets property values for cells falling within different numeric ranges.
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
#' @inherit by_value return
#'
#' @family `by` functions
#' @seealso [set-by]
#'
#' @export
#'
#' @examples
#' ht <- huxtable(c(1, 3, 5))
#' ht <- set_background_color_by(ht, by_range(c(2, 4), c("red", "yellow", "blue")))
#' ht
#' set_background_color_by(ht, by_range(c(2, 4), "pink", extend = FALSE))
#'
#' set_background_color_by(ht, by_range(c(1, 5), c("red", "yellow", "green"), right = TRUE))
#' set_background_color_by(ht, by_range(c(1, 5), c("red", "yellow", "green"), right = FALSE))
by_range <- function (breaks, values, right = FALSE, extend = TRUE) {
  assert_that(is.numeric(breaks))
  assert_that(all(breaks == sort(breaks)))
  if (extend) breaks <- c(-Inf, breaks, Inf)
  assert_that(length(values) == length(breaks) - 1)
  force(right)

  ranges_fun <- function(ht, rows, cols, current) {
    res <- current
    mx <- as.matrix(ht)[rows, cols]
    # avoid "NAs introduced by coercion"
    which_val <- suppressWarnings(findInterval(mx, breaks, left.open = right))
    which_val[is.na(which_val)] <- 0
    which_val[which_val == length(breaks)] <- 0
    res[which_val > 0] <- values[which_val[which_val > 0]]
    res
  }

  ranges_fun
}


#' Set properties by quantile groups
#'
#' These functions split cell values by quantiles. Non-numeric cells are ignored.
#'
#' @inheritParams by_range
#' @param quantiles Vector of quantiles.
#' @param values Vector of values. `length(values)` should be one greater than `length(quantiles)`.
#'
#' @details
#' `by_equal_size(n, values)` is a shortcut for `by_quantile(seq(1/n, 1 - 1/n, 1/n), values)`.
#' @family `by` functions
#' @seealso [set-by]
#' @inherit by_value return
#' @export
#'
#' @examples
#' ht <- hux(rnorm(5), rnorm(5))
#' set_background_color_by(ht, by_quantile(c(0.2, 0.8), c("red", "white", "green")))
#' set_background_color_by(ht, by_equal_size(3, c("red", "yellow", "green")))
by_quantile <- function (quantiles, values, right = TRUE) {
  assert_that(is.numeric(quantiles), all(quantiles <= 1), all(quantiles >= 0),
      all(quantiles == sort(quantiles)))
  assert_that(length(values) == length(quantiles) + 1)
  force(right)

  qr_fun <- function (ht, rows, cols, current) {
    vals <- as.matrix(ht)[rows, cols]
    vals <- suppressWarnings(as.numeric(vals))
    q_breaks <- quantile(vals, quantiles, na.rm = TRUE, names = FALSE)
    rf <- by_range(q_breaks, values, right = right, extend = TRUE)
    rf(ht, rows, cols, current)
  }

  qr_fun
}


#' @param n Number of equal-sized groups. `length(values)` should equal `n`.
#'
#' @rdname by_quantile
#' @export
by_equal_size <- function (n, values) {
  by_quantile(seq(1/n, 1 - 1/n, 1/n), values)
}


#' Set properties for cells that match a string or regular expression
#'
#' @param ... A list of name-value pairs. The names are regular expressions.
#' @param .grepl_args A list of arguments to pass to [grepl()]. Useful options
#'   include `fixed`, `perl` and `ignore.case`.
#'
#' @family `by` functions
#' @seealso [set-by]
#' @inherit by_value return
#' @export
#'
#' @examples
#' ht <- hux("The cat sat", "on the", "mat")
#' set_bold_by(ht, by_matching('at' = TRUE))
#' set_bold_by(ht, by_matching('a.*a' = TRUE))
#' set_bold_by(ht, by_matching('the' = TRUE, .grepl_args = list(ignore.case = TRUE)))
by_matching <- function(..., .grepl_args = list()) {
  vals <- c(...)
  patterns <- names(vals)

  matching_fun <- function (ht, rows, cols, current) {
    res <- current
    if (length(default) > 0) res[] <- default
    my_args <- .grepl_args
    for (pt in patterns) {
      my_args$pattern <- pt
      my_args$x <- as.matrix(ht)[rows, cols]
      matches <- do.call(grepl, my_args)
      res[matches] <- vals[[pt]]
    }
    res
  }

  return(matching_fun)
}
