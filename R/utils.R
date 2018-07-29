

# utility functions---------------------------------------------------------------------------------

#' @import assertthat
NULL


ncharw <- function (x) nchar(x, type = 'width')


# pinched from rlang
`%||%` <- function (x, y) {
  if (is.null(x)) y else x
}


blank_where <- function (text, cond) {
  text[cond] <- ''
  text
}


# pinched from HMS. Registers the method or sets a hook to register it on load of other package
register_s3_method <- function (pkg, generic, class = 'huxtable') {
  assert_that(is.string(pkg), is.string(generic))
  fun <- get(paste0(generic, '.', class), envir = parent.frame())

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, 'onLoad'), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}


assert_package <- function (fun, package) {
  if (! requireNamespace(package, quietly = TRUE)) stop(glue::glue(
        '{fun} requires the "{package}" package. To install, type:\n',
        'install.packages("{package}")'))
}


# return character matrix of formatted contents, suitably escaped
clean_contents <- function(ht, type = c('latex', 'html', 'screen', 'markdown', 'word', 'excel'), ...) {
  type <- match.arg(type)
  contents <- as.matrix(as.data.frame(ht))

  for (col in seq_len(ncol(contents))) {
    for (row in seq_len(nrow(contents))) {
      cell <- contents[row, col]
      num_fmt <- number_format(ht)[[row, col]] # a list element, double brackets
      if (! is.na(cell)) cell <- format_numbers(cell, num_fmt)
      if (is.na(cell)) cell <- na_string(ht)[row, col]
      contents[row, col] <- as.character(cell)
    }
    if (type %in% c('latex', 'html')) {
      to_esc <- escape_contents(ht)[, col]
      contents[to_esc, col] <-  sanitize(contents[to_esc, col], type)
    }
    # has to be after sanitization because we add &nbsp; for HTML (and non-space stuff for LaTeX):
    # later we can just use align for this:
    pad_chars <- pad_decimal(ht)[, col]
    align_pad   <- ncharw(align(ht)[, col]) == 1
    pad_chars[align_pad] <- align(ht)[align_pad, col]
    contents[, col] <- decimal_pad(contents[, col], pad_chars, type)
  }

  contents
}


format_color <- function (r_color, default = 'white') {
  r_color[is.na(r_color)] <- default
  apply(grDevices::col2rgb(r_color), 2, paste0, collapse = ', ')
}


# returns two rows(+1),cols(+1) arrays of border widths
collapsed_borders <- function (ht) {
  lb <- rb <- matrix(0, nrow(ht), ncol(ht))
  tb <- bb <- matrix(0, nrow(ht), ncol(ht))

  dc <- display_cells(ht, all = TRUE)
  # provides large speedup:
  dc <- as.matrix(dc[, c('row', 'col', 'display_row', 'display_col', 'end_row', 'end_col')])
  for (i in seq_len(nrow(ht))) for (j in seq_len(ncol(ht))) {
    dcell <- dc[ dc[, 'row'] == i & dc[, 'col'] == j, ]
    drow <- dcell['display_row']
    dcol <- dcell['display_col']
    # if we're in top row, set top border; bottom row set bb etc.
    if (i == drow)          tb[i, j] <- top_border(ht)[drow, dcol]
    if (i == dcell['end_row']) bb[i, j] <- bottom_border(ht)[drow, dcol]
    if (j == dcol)          lb[i, j] <- left_border(ht)[drow, dcol]
    if (j == dcell['end_col']) rb[i, j] <- right_border(ht)[drow, dcol]
  }
  lb <- cbind(lb, 0)
  rb <- cbind(0, rb)
  tb <- rbind(tb, 0)
  bb <- rbind(0, bb)
  result <- list()
  result$vert <- pmax(lb, rb)
  result$horiz <- pmax(tb, bb)

  result
}


# returns two rows(+1),cols(+1) arrays of border colors. right and top borders have priority.
# A border of 0 can still have a color.
collapsed_border_colors <- function (ht) {
  lb <- rb <- matrix(NA, nrow(ht), ncol(ht))
  tb <- bb <- matrix(NA, nrow(ht), ncol(ht))

  dc <- display_cells(ht, all = TRUE)
  # provides large speedup:
  dc <- as.matrix(dc[, c('row', 'col', 'display_row', 'display_col', 'end_row', 'end_col')])
  for (i in seq_len(nrow(ht))) for (j in seq_len(ncol(ht))) {
    dcell <- dc[ dc[, 'row'] == i & dc[, 'col'] == j, ]
    drow <- dcell['display_row']
    dcol <- dcell['display_col']
    # if we're in top row, set top border; bottom row set bb etc.
    if (i == drow)          tb[i, j] <- top_border_color(ht)[drow, dcol]
    if (i == dcell['end_row']) bb[i, j] <- bottom_border_color(ht)[drow, dcol]
    if (j == dcol)          lb[i, j] <- left_border_color(ht)[drow, dcol]
    if (j == dcell['end_col']) rb[i, j] <- right_border_color(ht)[drow, dcol]
  }
  lb <- cbind(lb, NA)
  rb <- cbind(NA, rb)
  tb <- rbind(tb, NA)
  bb <- rbind(NA, bb)
  result <- list()
  result$vert <- rb
  result$vert[is.na(rb)] <- lb[is.na(rb)]
  result$horiz <- bb
  result$horiz[is.na(bb)] <- tb[is.na(bb)]

  result
}


# find each numeric substring, and replace it:
format_numbers <- function (string, num_fmt) {
  if (! is.function(num_fmt) && is.na(num_fmt)) return(string) # ! is.function avoids a warning if num_fmt is a function

  format_numeral <- if (is.function(num_fmt)) num_fmt else
        if (is.character(num_fmt)) function (numeral) sprintf(num_fmt, numeral) else
        if (is.numeric(num_fmt)) function (numeral) formatC(round(numeral, num_fmt), format = 'f',
          digits = num_fmt) else
        stop('Unrecognized type of number_format: should be function, character or integer. See ?number_format')
  # Breakdown:
  # -?                    optional minus sign
  # [0-9]*                followed by any number of digits
  # \\.?                  optionally followed by a decimal
  # [0-9]+                which may also be followed by any number of digits
  # ([eE]-?[0-9]+)?       optionally including e or E as in scientific notation
  #                       along with (optionally) a sign preceding the digits
  #                       specifying the level of the exponent.
  stringr::str_replace_all(string,  '-?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?', function (x) format_numeral(as.numeric(x)))
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


decimal_pad <- function(col, pad_chars, type) {
  # where pad_chars is NA we do not pad
  orig_col  <- col
  na_pad    <- is.na(pad_chars)
  col       <- col[! na_pad]
  pad_chars <- pad_chars[! na_pad]
  if (length(col) == 0) return(orig_col)

  find_pos  <- function(string, char) {
    regex <- gregexpr(char, string, fixed = TRUE)[[1]]
    regex[length(regex)]
  }
  pos <- mapply(find_pos, col, pad_chars)
  nchars <- nchar(col, type = 'width')
  # take the biggest distance from the decimal point
  pos[pos == -1L] <- nchars[pos == -1L] + 1
  chars_after_. <- nchars - pos

  pad_n_spaces <- max(chars_after_.) - chars_after_.
  # use non-breaking space on screen also
  pad_char <- switch(type, 'html' = '&nbsp;', 'latex' = '~', 'screen' = '\u00a0', ' ')
  col <- paste0(col, str_rep(pad_char, pad_n_spaces))

  orig_col[! na_pad] <- col
  orig_col
}


check_positive_dims <- function (ht) {
  if (nrow(ht) < 1) {
    warning("huxtable has zero rows")
    return(FALSE)
  }
  if (ncol(ht) < 1) {
    warning("huxtable has zero columns")
    return(FALSE)
  }

  return(TRUE)
}


# return data frame mapping real cell positions to cells displayed. `all = TRUE` returns all
# cells, including those shadowed by others.
# columns are row, col (of real cell);
# shadowed if cell is covered by another, the 'display cell'; if not, it is its own 'display cell'
# display_row, display_col, rowspan, colspan of 'display cell';
# end_row, end_col end of that display cell.
#
# PLAN:
# start with four matrices dispr, dispc, endr, endc of row(ht) and col(ht)
# for each cell with colspan/rowspan > 1, set dispr/c to its row/col for its 'display area'
# and set endr, endc to the bottom right cell for its display area
# shadowed is dispr != r or dispc != c
# c() the results down the data frame columns;
#
# QUESTION: what if there are overlapping areas? Shouldn't that always be avoided?

display_cells <- function (ht, all = TRUE, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  rowspan <- new_rowspan
  colspan <- new_colspan
  display_row <- end_row <- row <- row(ht)
  display_col <- end_col <- col <- col(ht)
  displayers <- rowspan > 1 | colspan > 1
  touched <- shadowed <- matrix(FALSE, nrow(ht), ncol(ht))
  for (idx in which(displayers)) {
    rr <- row[idx]
    cc <- col[idx]
    end_r   <- rr + rowspan[idx] - 1
    end_c   <- cc + colspan[idx] - 1
    da_rows <- seq(rr, end_r)
    da_cols <- seq(cc, end_c)
    if (any(touched[da_rows, da_cols])) stop(glue::glue('Overlapping multirow/multicolumn cells in',
          ' [{da_rows}, {da_cols}] of huxtable'))
    display_row[da_rows, da_cols] <- rr
    display_col[da_rows, da_cols] <- cc
    rowspan[da_rows, da_cols] <- rowspan[idx]
    colspan[da_rows, da_cols] <- colspan[idx]
    end_row[da_rows, da_cols] <- end_r
    end_col[da_rows, da_cols] <- end_c
    shadowed[da_rows, da_cols] <- TRUE
    touched[da_rows, da_cols]  <- TRUE
    shadowed[rr, cc] <- FALSE
  }

  dcells <- data.frame(
          row         = c(row),
          col         = c(col),
          rowspan     = c(rowspan),
          colspan     = c(colspan),
          display_row = c(display_row),
          display_col = c(display_col),
          shadowed    = c(shadowed),
          end_row     = c(end_row),
          end_col     = c(end_col)
        )
  if (! all) dcells <- dcells[! dcells$shadowed, ]

  return(dcells)
}


get_caption_hpos <- function (ht) {
  hpos <- sub('.*(left|center|right)', '\\1', caption_pos(ht))
  if (! hpos %in% c('left', 'center', 'right')) hpos <- position(ht)

  hpos
}


real_align <- function(ht) {
  # align(ht) can be e.g. "." for aligning on a decimal point
  al <- align(ht)
  al[! al %in% c('left', 'center', 'right')] <- 'right'

  al
}


smart_hux_from_df <- function(dfr) {
  col_nchars <- sapply(dfr, function (col) max(nchar(as.character(col), type = "width")))

  ht <- as_hux(dfr, add_colnames = TRUE, autoformat = TRUE)

  wrap(ht)[-1, col_nchars > 15] <- TRUE
  width <- sum(col_nchars) / 90
  width(ht) <- min(1, max(0.2, width))
  # this did not seem to improve on LaTeX's own attempts
  # prop_widths  <- col_nchars/sum(col_nchars)
  # equal_widths <- rep(1/ncol(ht), ncol(ht))
  # col_width(ht) <- round((prop_widths * .75 + equal_widths * .25), 2)

  ht
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
#' @seealso huxtable-options
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

#' Huxtable logo
#'
#' @param latex Use LaTeX names for fonts.
#' @return The huxtable logo.
#' @export
#'
#' @examples
#' print_screen(hux_logo())
#'
hux_logo <- function(latex = FALSE) {
  assert_that(is.flag(latex))
  logo <- hux(c('h', NA), c('u', 'table'), c('x', NA))
  rowspan(logo)[1, 1] <- 2
  colspan(logo)[2, 2] <- 2
  logo <- set_all_borders(logo, 0.5)
  font_size(logo) <- if (latex) 11 else 20
  font_size(logo)[1, 2:3] <- if (latex) 14 else 24
  font_size(logo)[1, 1] <-  if (latex) 24 else 42
  background_color(logo)[1, 1] <- '#e83abc'
  background_color(logo)[1, 3] <- 'black'
  text_color(logo)[1, 3] <- 'white'
  width(logo) <- if (latex) '0.21\\textwidth' else '60pt'
  height(logo) <- if (latex) '45pt' else '60pt'
  font(logo) <- 'Palatino'
  if (latex) font(logo) <- 'ppl'
  top_padding(logo) <- 2
  bottom_padding(logo) <- 2
  bottom_padding(logo)[2, 2] <- 1
  align(logo)[2, 2] <- 'center'
  col_width(logo) <- c(.4, .3, .3)
  position(logo) <- 'center'
  logo
}
