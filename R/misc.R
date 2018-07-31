
# miscellaneous public tools------------------------------------------------------------------------

#' @import assertthat
NULL


#' Insert one matrix into another.
#'
#' These functions combine two matrix-like objects and return the result.
#'
#' @param x A matrix-like object, e.g. a huxtable
#' @param y Matrix or vector to be inserted into `x`
#' @param after Row or column after which `y` is inserted. Can be 0. Can be a row or column name.
#'   By default, inserts `y` after the end of `x`.
#' @param ... Arguments passed to [rbind()] or [cbind()]
#'
#' @return For `add_rows`, the result of `rbind(x[1:after,], y, x[-(1:after),]`. For `add_columns`
#'   the same but with columns. `after = 0` and `after = nrow(x)` or `ncol(x)` are handled correctly.
#' @export
#'
#' @details
#' For `huxtable` objects, arguments in `...` can include `copy_cell_props`.
#'
#' You cannot insert data frames into huxtables using this method, because you can't
#' `cbind` huxtables and data frames. (See the "Dispatch" section in [cbind()] for details of why
#' not.)
#'
#' @seealso [insert_row()] and [insert_column()], which insert multiple values into a single row.
#'
#' @examples
#' ht <- hux(Jam = c('Blackberry', 'Strawberry'), Price = c(1.90, 1.80), add_colnames = TRUE)
#' ht2 <- hux('Gooseberry', 2.10)
#' add_rows(ht, ht2)
#' add_rows(ht, ht2, after = 1)
#' mx <- matrix(c('Sugar', '50%', '60%', 'Weight (g)', 300, 250), 3, 2)
#' add_columns(ht, mx, after = 'Jam')
add_rows <- function (x, y, after = nrow(x), ...) {
  add_row_cols(x, y, after, dimno = 1, ...)
}


#' @export
#' @rdname add_rows
#' @examples
#' ht <- hux(a = 1:3, b = 1:3)
#' ht2 <- hux(d = letters[1:3])
#' add_columns(ht, ht2, after = "a")
add_columns <- function (x, y, after = ncol(x), ...) {
  add_row_cols(x, y, after, dimno = 2, ...)
}


add_row_cols <- function (x, y, after, dimno, ...) {
  dims <- dim(x)
  end_idx <- dims[dimno]
  assert_that(is.numeric(dims))
  if (is.character(after)) {
    after <- match(after, dimnames(x)[[dimno]])
  }
  assert_that(is.number(after), after >= 0, after <= end_idx)

  second_idxes <- if (after < end_idx) seq(after + 1, end_idx) else integer(0)
  if (dimno == 1) {
    rbind(x[seq_len(after),], y, x[second_idxes,], ...)
  } else {
    cbind(x[, seq_len(after)], y, x[, second_idxes], ...)
  }
}


#' Insert a row or column
#'
#' These convenience functions wrap `cbind` or `rbind` for huxtables to insert
#' a single row.
#' @param ht A huxtable.
#' @param ... Cell contents.
#' @param after Insert the row/column after this position. 0 (the default) inserts as the first row/column.
#' @param copy_cell_props Copy cell properties from the previous row or column (if after > 0). See [cbind.huxtable()].
#' @details
#' In `insert_column` only, you can use a column name for `after`.
#' @return The modified huxtable
#' @seealso [add_rows()] and [add_columns()], which are more general.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, c = 1:5)
#' insert_row(ht, 2.5, 2.5, 2.5, after = 2)
#' insert_column(ht, 5:1)
#' insert_column(ht, 5:1, after = 3)
#' insert_column(ht, 5:1, after = "b")
insert_column <- function (ht, ..., after = 0, copy_cell_props = TRUE) {
  # is.count would complain about 0
  assert_that(is.scalar(after), is.number(after) || is.string(after))
  if (is.number(after)) assert_that(after >= 0, after <= ncol(ht))
  if (is.string(after)) {
    assert_that(has_name(ht, after))
    after <- match(after, colnames(ht))
  }

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[, seq(1, after, 1)]
  }
  if (after < ncol(ht)) {
    ht2 <- ht[, seq(after + 1, ncol(ht), 1)]
  }
  to_insert <- c(...)
  res <- if (! is.null(ht1)) cbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) cbind(res, ht2) else res

  res
}


#' @rdname insert_column
#'
#' @export
insert_row <- function (ht, ..., after = 0, copy_cell_props = TRUE) {
  # is.count would complain about 0
  assert_that(is.scalar(after), is.number(after), after >= 0, after <= nrow(ht))

  ht1 <- NULL
  ht2 <- NULL
  if (after > 0) {
    ht1 <- ht[seq(1, after, 1), ]
  }
  if (after < nrow(ht)) {
    ht2 <- ht[seq(after + 1, nrow(ht), 1), ]
  }
  to_insert <- c(...)
  res <- if (! is.null(ht1)) rbind(ht1, to_insert, copy_cell_props = copy_cell_props) else to_insert
  res <- if (! is.null(ht2)) rbind(res, ht2) else res

  res
}


#' Add a row with a footnote
#'
#' This adds a single row at the bottom. The first cell contains the footnote; it spans
#' all table columns and has an optional border above.
#' @param ht A huxtable.
#' @param text Text for the footnote.
#' @param border Width of the footnote's top border. Set to 0 for no border.
#' @param ... Other properties, passed to [set_cell_properties()] for the footnote cell.
#'
#' @return The modified huxtable
#' @export
#'
#' @examples
#' ht <- hux(a = 1:5, b = 1:5, d = 1:5)
#' ht <- add_footnote(ht, '* this is a footnote')
#' ht
add_footnote <- function(ht, text, border = 0.8, ...) {
  nr <- nrow(ht) + 1
  nc <- ncol(ht)
  ht <- rbind(ht, rep('', nc), copy_cell_props = FALSE)
  ht[nr, 1] <- text
  colspan(ht)[nr, 1] <- nc
  ht <- set_left_border(ht, nr, 1, 0)
  ht <- set_right_border(ht, nr, 1, 0)
  ht <- set_bottom_border(ht, nr, 1, 0)
  ht <- set_top_border(ht, nr, 1, border)
  wrap(ht)[nr, 1] <- TRUE
  if (! missing(...)) ht <- set_cell_properties(ht, nr, 1, ...)

  ht
}



#' Sanitize table elements
#'
#' This is copied over from `xtable::sanitize()`.
#'
#' @param str A character object.
#' @param type `"latex"` or `"html"`.
#'
#' @return The sanitized character object.
#' @export
#'
#' @examples
#' foo <- 'Make $$$ with us'
#' sanitize(foo, type = 'latex')
sanitize <- function (str, type = "latex") {
  if (type == "latex") {
    result <- str
    result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
    result <- gsub("$", "\\$", result, fixed = TRUE)
    result <- gsub(">", "$>$", result, fixed = TRUE)
    result <- gsub("<", "$<$", result, fixed = TRUE)
    result <- gsub("|", "$|$", result, fixed = TRUE)
    result <- gsub("{", "\\{", result, fixed = TRUE)
    result <- gsub("}", "\\}", result, fixed = TRUE)
    result <- gsub("%", "\\%", result, fixed = TRUE)
    result <- gsub("&", "\\&", result, fixed = TRUE)
    result <- gsub("_", "\\_", result, fixed = TRUE)
    result <- gsub("#", "\\#", result, fixed = TRUE)
    result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
    result <- gsub("~", "\\~{}", result, fixed = TRUE)
    result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$",
      result, fixed = TRUE)
    return(result)
  }
  else {
    result <- str
    result <- gsub("&", "&amp;", result, fixed = TRUE)
    result <- gsub(">", "&gt;", result, fixed = TRUE)
    result <- gsub("<", "&lt;", result, fixed = TRUE)
    return(result)
  }
}


#' Huxtable logo
#'
#' Returns a randomized huxtable logo, inspired by Mondrian.
#'
#' @param latex Style for LaTeX.
#' @param html  Style for HTML.
#' @return The huxtable logo.
#' @export
#'
#' @examples
#' print_screen(hux_logo())
#'
hux_logo <- function(latex = FALSE, html = FALSE) {
  assert_that(is.flag(latex))

  blank <- if (html) '&nbsp;' else ''
  squares <- rep(blank, 36)
  letter_squares <- sort(sample(36, 8))
  squares[letter_squares] <- strsplit('huxtable', '')[[1]]
  mx <- matrix(squares, 6, 6, byrow = TRUE)
  letter_squares <- which(mx != blank) # back in vertical space
  h_square <- which(mx == 'h')

  mondrian <- as_hux(mx)
  escape_contents(mondrian) <- FALSE
  align(mondrian) <- 'centre'
  font(mondrian) <- 'Arial'
  if (latex) font(mondrian) <- 'cmss'
  mondrian <- set_all_borders(mondrian, if (html) 2 else 1.2)
  mondrian <- set_all_padding(mondrian, 0)
  mondrian <- set_all_border_colors(mondrian, 'black')
  background_color(mondrian)[sample(36, 8)] <- sample(c('red', 'blue', 'yellow'), 8, replace = TRUE)
  bold(mondrian)[h_square] <- TRUE

  colspan_ok <- setdiff(1:30, letter_squares - 6)
  colspan2 <- rep(- 6, 2)
  for (i in 1:2) {
    colspan2[i] <- sample(colspan_ok, 1)
    colspan_ok  <- setdiff(colspan_ok, c(colspan2 - 6, colspan2 + 6))
  }

  # -7 to avoid being top-left of any letter_squares (as we may get 2x2 cells)
  # also avoid breaking colspans
  rowspan_ok <- setdiff(1:36, c(1:6*6, letter_squares - 1, letter_squares - 7, colspan2 - 1,
        colspan2 + 5, colspan2 + 6, colspan2))
  rowspan2 <- rep(- 1, 3)
  for (i in 1:3) {
    rowspan2[i] <- sample(rowspan_ok, 1)
    rowspan_ok <- setdiff(rowspan_ok, c(rowspan2 - 1, rowspan2 + 1))
  }

  colspan(mondrian)[colspan2] <- 2
  rowspan(mondrian)[rowspan2] <- 2

  if (html) {
    mondrian <- set_all_padding(mondrian, 2)
    width(mondrian)  <- '120pt'
    height(mondrian) <- '120pt'
    col_width(mondrian)  <- '20pt'
    row_height(mondrian) <- '20pt'
  }

  mondrian
}


#' Default print method for huxtables
#'
#' By default huxtables are printed using [print_screen()]. In certain cases, for example
#' in Sweave documents, it may be
#' useful to change this. You can do so by setting `options("huxtable.print")`.
#' @param x A huxtable.
#' @param ... Options passed to other methods.
#'
#' @return `print` prints the huxtable and returns `NULL` invisibly.
#' @export
#'
#' @seealso To change how huxtables are printed within `knitr`, see
#'   `options("huxtable.knitr_output_format")` in [huxtable-options]
#' @examples
#' \dontrun{
#' # to print LaTeX output:
#' options(huxtable.print = print_latex)
#' # to print huxtables like data frames:
#' options(huxtable.print = function(x, ...) print(as.data.frame(x)))
#' }
print.huxtable <- function(x, ...) {
  meth <- getOption('huxtable.print', default = print_screen)
  if (is.character(meth)) meth <- eval(as.symbol(meth))

  meth(x, ...)
}


#' @rdname print.huxtable
#' @param output One of `"html"`, `"latex"`, `"md"` or `"screen"`
#'
#' @return `format` returns a string representation from [to_latex()], [to_html()] etc.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:3, b = 4:6)
#' format(ht, output = 'screen')
#' format(ht, output = 'md')
format.huxtable <- function(x, ..., output) {
  assert_that(is.string(output))
  assert_that(output %in% c('latex', 'html', 'md', 'screen'))
  fn <- paste0('to_', output)
  do.call(fn, list(ht = x, ...))
}
