

# utility functions-----------------------------------------------------------------------------------------------------

#' @import assertthat
NULL



# return character matrix of formatted contents, suitably escaped
clean_contents <- function(ht, type = c('latex', 'html', 'screen', 'markdown', 'word', 'excel'), ...) {
  type <- match.arg(type)
  contents <- as.matrix(as.data.frame(ht))

  for (col in 1:ncol(contents)) {
    for (row in 1:nrow(contents)) {
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
    # has to be after sanitization because we add &nbsp; for HTML
    contents[, col] <- decimal_pad(contents[, col], pad_decimal(ht)[, col], type)
  }

  contents
}


format_color <- function (r_color, default = 'white') {
  if (is.na(r_color)) r_color <- default
  paste0(as.vector(grDevices::col2rgb(r_color)), collapse = ', ')
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
  # -?(\\d*\\.)?\\d+         Optional minus sign, followed by optional digits and decimal point,
  #                          followed by one or more digits. Matches "1", "1.1", ".1", "-1", "-1.1", "-.1"
  # (?<! ... )               But not if all of this comes straight after ...
  # \\d(e|E)-?\\d{1,3}       a digit, an e or E, an optional minus, and 0 to 3 more digits.
  #                          We'd like to have \d* here but you can't have unbounded-length regexes.
  #                          The up to 3 digits rules out exponents with up to 1000 zeros, so we should
  #                          be good...
  stringr::str_replace_all(string,  '(?<!\\d(e|E)-?\\d{0,3})-?(\\d*\\.)?\\d+', function (x) format_numeral(as.numeric(x)))
}



#' Sanitize table elements
#'
#' This is copied over from [xtable::sanitize()].
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


# return data frame mapping real cell positions to cells displayed. `all = TRUE` returns all
# cells, including those shadowed by others.
display_cells <- function(ht, all = TRUE, new_rowspan = rowspan(ht), new_colspan = colspan(ht)) {
  dcells <- data.frame(
          row     = rep(seq_len(nrow(ht)), ncol(ht)),
          col     = rep(seq_len(ncol(ht)), each = nrow(ht)),
          rowspan = as.vector(new_rowspan),
          colspan = as.vector(new_colspan)
        )
  dcells$display_row <- dcells$row
  dcells$display_col <- dcells$col
  dcells$shadowed <- FALSE

  change_cols <- c('display_row', 'display_col', 'rowspan', 'colspan')
  for (i in 1:nrow(dcells)) {
    if (dcells$rowspan[i] == 1 && dcells$colspan[i] == 1) next
    if (dcells$shadowed[i]) next

    dr <- dcells$row[i]
    dc <- dcells$col[i]
    spanned <- dcells$row %in% dr:(dr + dcells$rowspan[i] - 1) & dcells$col %in% dc:(dc + dcells$colspan[i] - 1)
    dcells[spanned, change_cols] <- matrix(as.numeric(dcells[i, change_cols]), sum(spanned), length(change_cols),
          byrow = TRUE)

    shadowed <- spanned & (1:nrow(dcells)) != i
    dcells$shadowed[shadowed] <- TRUE
  }
  dcells$end_row <- dcells$display_row + dcells$rowspan - 1
  dcells$end_col <- dcells$display_col + dcells$colspan - 1

  if (! all) dcells <- dcells[! dcells$shadowed, ]

  dcells
}


get_caption_hpos <- function (ht) {
  hpos <- sub('.*(left|center|right)', '\\1', caption_pos(ht))
  if (! hpos %in% c('left', 'center', 'right')) hpos <- position(ht)

  hpos
}


#' Print a huxtable within knitr
#'
#' @param x A huxtable.
#' @param options Not used.
#' @param ... Not used.
#'
#' @details
#' knitr calls [knitr::knit_print()] on objects when they are printed in a knitr (or RMarkdown) document.
#' The default method for `huxtable` objects guesses the appropriate output format and
#' prints itself out appropriately.
#'
#' @family knit_print
#' @importFrom knitr knit_print
#' @export
knit_print.huxtable <- function (x, options, ...) {
  of <- guess_knitr_output_format()
  call_name <- switch(of, latex = 'to_latex', html = 'to_html', 'to_screen')
  res <- do.call(call_name, list(ht = x))
  if (of == 'latex') {
    latex_deps <- report_latex_dependencies(quiet = TRUE)
    tenv <- tabular_environment(x)
    if (tenv %in% c('tabulary', 'longtable')) latex_deps <- c(latex_deps, list(rmarkdown::latex_dependency(tenv)))
    return(knitr::asis_output(res, meta = latex_deps))
  } else if (of == 'html') {
    res <- knitr::asis_output(htmltools::htmlPreserve(res))
    return(res)
  } else {
    return(knitr::asis_output(res))
  }
}

# see zzz.R
#' Print data frames in knitr using huxtable
#'
#' @inherit knit_print.huxtable params
#'
#' @details
#' `huxtable` defines a `knit_print` method for `data.frame`s. This converts the data frame
#' to a huxtable, with `add_colnames = TRUE`, themes it using [theme_plain()] and prints it.
#' To turn this behaviour off, set `options(huxtable.knit_print_df = FALSE)`. To change the theme, set
#' `options("huxtable.knit_print_df_theme")` to a one-argument function which should return the huxtable.
#'
#' @importFrom knitr knit_print
#' @export
#' @family knit_print
#' @examples
#' \dontrun{
#' # in your knitr document
#' mytheme <- function (ht) {
#'   ht <- set_all_borders(ht, 0.4)
#'   ht <- set_all_border_colors(ht, "darkgreen")
#'   ht <- set_background_color(ht, evens, odds, "salmon")
#'   ht
#' }
#'
#' options(huxtable.knit_print_df_theme = mytheme)
#' data.frame(a = 1:5, b = 1:5) # groovy!
#' }
knit_print.data.frame <- function(x, options, ...) {
  if (! isTRUE(getOption('huxtable.knit_print_df', TRUE))) {
    NextMethod()
  } else {
    ht <- smart_hux_from_df(x)
    df_theme <- getOption('huxtable.knit_print_df_theme', theme_plain)
    ht <- df_theme(ht)
    knit_print(ht) # we are now hopping down the class hierarchy, so do this rather than NextMethod()
  }
}

# could become public; or could become part of as_hux (in which case need to drop the recursion!)
# but NB: 2 ideas, 1 is that e.g. integers etc. should have appropriate defaults; the other is making ideal
# widths etc. which is much more of an "art"
smart_hux_from_df <- function(dfr) {
  numeric_cols <- sapply(dfr, function (col) any(c("numeric", "complex") %in% class(col)))
  integer_cols <- sapply(dfr, function (col) "integer" %in% class(col))
  date_cols <- sapply(dfr, function (col) "Date" %in% class(col))
  col_nchars <- sapply(dfr, function (col) max(nchar(as.character(col), type = "width")))

  ht <- as_hux(dfr, add_colnames = TRUE)
  number_format(ht)[-1, ! numeric_cols]                  <- NA
  number_format(ht)[1, ]                                 <- NA
  pad_decimal(ht)[-1, numeric_cols]                      <- '.'
  number_format(ht)[-1, integer_cols]                    <- 0
  wrap(ht)[-1, col_nchars > 15]                          <- TRUE
  align(ht)[, numeric_cols | integer_cols | date_cols]   <- 'right'
  width <- sum(col_nchars) / 90
  width(ht)                                              <- min(1, max(0.2, width))
  # this did not seem to improve on LaTeX's own attempts
  # prop_widths  <- col_nchars/sum(col_nchars)
  # equal_widths <- rep(1/ncol(ht), ncol(ht))
  #col_width(ht)                                          <- round((prop_widths * .75 + equal_widths * .25), 2)
  ht
}


#' Default print method for huxtables
#'
#' By default huxtables are printed using [print_screen()]. In certain cases, for example
#' in Sweave documents, it may be
#' useful to change this. You can do so by setting `options(huxtable.print)`.
#' @param x A huxtable.
#' @param ... Options passed to other methods.
#'
#' @export
#'
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


#' Guess knitr output format
#'
#' Convenience function which tries to guess the ultimate output from knitr and rmarkdown.
#'
#' @return 'html', 'latex', or something else. If we are not in a knitr document, returns an empty string.
#' @export
#'
#' @examples
#' \dontrun{
#' # in a knitr document
#' guess_knitr_output_format()
#' }
guess_knitr_output_format <- function() {
  # this is on hold until I'm sure I want 'markdown' to be interpreted as HTML
  # if (utils::packageVersion('knitr') >= '1.17.8') {
  #   # delegate to knitr
  #   if (knitr::is_latex_output()) return('latex')
  #   if (knitr::is_html_output()) return('html')
  #   return('')
  # }
  of <- knitr::opts_knit$get('out.format')
  if (is.null(of) || of == 'markdown') {
    of <- knitr::opts_knit$get('rmarkdown.pandoc.to')
    if (is.null(of)) {
      knit_in <- knitr::current_input()
      if (is.null(knit_in)) return('')
      of <- rmarkdown::default_output_format(knit_in)
      of <- of$name
    }
  }
  if (of == 'tufte_handout') of <- 'latex'
  if (of == 'tufte_html') of <- 'html'
  of <- sub('_.*', '', of)
  if (of %in% c('ioslides', 'revealjs', 'slidy')) of <- 'html'
  if (of %in% c('beamer', 'pdf')) of <- 'latex'

  of
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
  font(logo) <- 'Palatino, Palatino Linotype, Palatino LT STD, Book Antiqua, Georgia, serif'
  if (latex) font(logo) <- 'ppl'
  top_padding(logo) <- 2
  bottom_padding(logo) <- 2
  bottom_padding(logo)[2, 2] <- 1
  align(logo)[2, 2] <- 'center'
  col_width(logo) <- c(.4, .3, .3)
  position(logo) <- 'center'
  logo
}


#' Quickly create a PDF, HTML, Word or Excel document showing matrices, data frames, et cetera.
#'
#' @param ... One or more huxtables or R objects with an `as_huxtable` method.
#' @param file File path for the output.
#' @param borders Border width for members of `...` that are not huxtables.
#'
#' @return Invisible `NULL`.
#'
#' @details Objects in `...` will be converted to huxtables, with borders added.
#'
#' If \sQuote{file} is not specified, the default file path is "huxtable-output.xxx" in
#' the working directory. If the session is interactive, you'll be asked to confirm any
#' overwrite; if the session is not interactive, the command will fail.
#'
#' @examples
#' \dontrun{
#' m <- matrix(1:4, 2, 2)
#' dfr <- data.frame(a = 1:5, b = 1:5)
#' quick_pdf(m, dfr)
#' quick_html(m, dfr)
#' quick_docx(m, dfr)
#' quick_xlsx(m, dfr)
#' }
#' @name quick-output
NULL


#' @rdname quick-output
#' @export
quick_pdf <- function (..., file = confirm("huxtable-output.pdf"), borders = 0.4) {
  assert_that(is.number(borders))
  force(file) # ensures confirm() is called before any other files are created.
  hts <- huxtableize(list(...), borders)
  # on my Mac, tempdir() gets a double slash in the path, which screws up texi2pdf.
  # You can't use normalizePath with a non-existent file, so the below doesn't work:
  # latex_file <- normalizePath(tempfile(fileext = ".tex"), mustWork = TRUE)
  clean_tmp_dir <- normalizePath(tempdir(), mustWork = TRUE)
  latex_file <- tempfile(tmpdir = clean_tmp_dir, fileext = ".tex")
  sink(latex_file)
  tryCatch({
      cat('\\documentclass{article}\n')
      report_latex_dependencies()
      cat('\n\\begin{document}')
      lapply(hts, function (ht) {
        cat('\n\n')
        print_latex(ht)
        cat('\n\n')
      })
      cat('\n\\end{document}')
    },
    error = identity,
    finally = {sink()}
  )

  tools::texi2pdf(latex_file, clean = TRUE) # outputs to current working directory
  pdf_file <- sub('\\.tex$', '.pdf', basename(latex_file))
  if (! file.exists(pdf_file)) stop('Could not find texi2pdf output file "', pdf_file, '"')
  if (! file.remove(latex_file)) warning('Could not remove intermediate TeX file "', latex_file, '"')
  # we overwrite existing files. If no explicit `file` argument was specified, confirm() has
  # already checked if this is OK, or has failed in non-interactive sessions:
  if (file.copy(pdf_file, file, overwrite = TRUE)) {
    file.remove(pdf_file)
  } else {
    stop('Could not copy pdf file to ', file, '. The pdf file remains at "', pdf_file, '"')
  }

  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_html <- function (..., file = confirm("huxtable-output.html"), borders = 0.4) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  sink(file)
  cat('<!DOCTYPE html><html><body>')
  tryCatch({
    lapply(hts, function (ht) {
      cat('<p>&nbsp;</p>')
      print_html(ht)
      cat('\n\n')
    })
    cat('</body></html>')
  },
    error = identity,
    finally = {sink()}
  )

  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_docx <- function (..., file = confirm("huxtable-output.docx"), borders = 0.4) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  my_doc <- officer::read_docx()
  for (ht in hts) {
    ft <- as_flextable(ht)
    my_doc <- flextable::body_add_flextable(my_doc, ft)
    my_doc <- officer::body_add_par(my_doc, " ")
  }
  print(my_doc, target = file)

  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_xlsx <- function (..., file = confirm("huxtable-output.xlsx"), borders = 0.4) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  wb <- openxlsx::createWorkbook()
  ix <- 0
  for (ht in hts) {
    ix <- ix + 1
    wb <- as_Workbook(ht, Workbook = wb, sheet = paste("sheet", ix))
  }
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  invisible(NULL)
}


huxtableize <- function (obj_list, borders) {
  lapply(obj_list, function (obj) {
    if (! inherits(obj, 'huxtable')) {
      obj <- as_huxtable(obj)
      obj <- set_all_borders(obj, borders)
    }
    obj
  })
}


confirm <- function (file) {
  if (! interactive()) stop('Please specify a `file` argument for non-interactive use of quick_xxx functions.')
  if (file.exists(file)) {
    answer <- readline(paste0('File "', file, '" already exists. Overwrite? [yN]'))
    if (! answer %in% c('y', 'Y')) stop('OK, stopping.')
  }
  file
}
