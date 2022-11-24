

#' @import assertthat
NULL


#' Add a row with a footnote
#'
#' This adds a single row at the bottom. The first cell contains the footnote; it spans
#' all table columns and has an optional border above.
#' @param ht A huxtable.
#' @param text Text for the footnote.
#' @param border Width of the footnote's top border. Set to 0 for no border, or
#'   `NULL` to leave the border unchanged.
#' @param number_format Number format for the footnote cell.
#' @param ... Other properties, passed to [set_cell_properties()] for the footnote cell.
#'
#' @return The modified huxtable
#' @export
#'
#' @examples
#' jams <- add_footnote(jams,
#'       "* subject to availability")
#' jams
add_footnote <- function(ht, text, border = 0.8, number_format = NA, ...) {
  nr <- nrow(ht) + 1
  nc <- ncol(ht)
  ht <- rbind(ht, rep("", nc), copy_cell_props = FALSE)
  ht[nr, 1] <- text
  colspan(ht)[nr, 1] <- nc
  ht <- set_left_border(ht, nr, 1, 0)
  ht <- set_right_border(ht, nr, 1, 0)
  ht <- set_bottom_border(ht, nr, 1, 0)
  if (! is.null(border)) ht <- set_top_border(ht, nr, everywhere, border)
  wrap(ht)[nr, 1] <- TRUE
  if (! missing(...)) ht <- set_cell_properties(ht, nr, 1, ...)
  ht <- set_number_format(ht, nr, 1, number_format)

  ht
}



#' Escape text for various formats
#'
#' This escapes a string for LaTeX, HTML or RTF.
#'
#' @param str A character object.
#' @param type `"latex"`, `"html"` or `"rtf"`.
#'
#' @return The sanitized character object.
#'
#' @details
#' HTML and LaTeX code was copied over from `xtable::sanitize()`.
#'
#' @export
#'
#' @examples
#'
#' txt <- "Make $$$ with us"
#' sanitize(txt, type = "latex")
sanitize <- function (str, type = c("latex", "html", "rtf")) {
  type <- match.arg(type)
  result <- str

  if (type == "latex") {
    result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
    result <- gsub("\n", " \\newline ", result, fixed = TRUE)
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
    result <- gsub("^", "\\textasciicircum ", result, fixed = TRUE)
    result <- gsub("~", "\\~{}", result, fixed = TRUE)
    result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$",
                result, fixed = TRUE)
  }
  else if (type == "html"){
    result <- gsub("&",  "&amp;", result, fixed = TRUE)
    result <- gsub(">",  "&gt;",  result, fixed = TRUE)
    result <- gsub("<",  "&lt;",  result, fixed = TRUE)
    result <- gsub("\n", "<br>",  result, fixed = TRUE)
  } else if (type == "rtf") {
    result <- gsub("\\", "\\\\",   result, fixed = TRUE)
    result <- gsub("{",  "\\{",    result, fixed = TRUE)
    result <- gsub("}",  "\\}",    result, fixed = TRUE)
    result <- gsub("\n", "\\line ", result, fixed = TRUE)
  }

  return(result)
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

  blank <- if (html) "&nbsp;" else if (latex) "~" else ""
  squares <- rep(blank, 36)
  letter_squares <- sort(sample(36, 8))
  squares[letter_squares] <- strsplit("huxtable", "")[[1]]
  mx <- matrix(squares, 6, 6, byrow = TRUE)
  letter_squares <- which(mx != blank) # back in vertical space
  h_square <- which(mx == "h")

  mondrian <- as_hux(mx, add_colnames = FALSE)
  escape_contents(mondrian) <- FALSE
  align(mondrian) <- "centre"
  font(mondrian) <- "DejaVu Sans"
  if (latex && ! getOption("huxtable.latex_use_fontspec", FALSE)) {
    font(mondrian) <- "cmss"
  }
  mondrian <- set_all_borders(mondrian, if (html) 2 else 1.2)
  mondrian <- set_all_padding(mondrian, 0)
  mondrian <- set_all_border_colors(mondrian, "black")
  bg_cells <- sample(36, 8)
  bg_colors <- sample(c("red", "blue", "yellow"), 8, replace = TRUE)
  background_color(mondrian)[bg_cells] <- bg_colors
  text_color(mondrian)[bg_cells] <- ifelse(bg_colors == "yellow", "black", "white")
  bold(mondrian)[h_square] <- TRUE

  colspan_ok <- setdiff(1:30, letter_squares - 6)
  colspan2 <- rep(- 6, 2)
  for (i in 1:2) {
    colspan2[i] <- sample(colspan_ok, 1)
    colspan_ok  <- setdiff(colspan_ok, c(colspan2 - 6, colspan2 + 6))
  }

  # -7 to avoid being top-left of any letter_squares (as we may get 2x2 cells)
  # also avoid breaking colspans
  rowspan_ok <- setdiff(1:36, c(1:6 * 6, letter_squares - 1, letter_squares - 7, colspan2 - 1,
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
    width(mondrian)  <- "120pt"
    height(mondrian) <- "120pt"
    col_width(mondrian)  <- "20pt"
    row_height(mondrian) <- "20pt"
  }
  if (latex) {
    wrap(mondrian) <- TRUE
  }

  mondrian
}


#' Format and print huxtables using a default method
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
#'   # to print LaTeX output:
#'   options(huxtable.print = print_latex)
#' }
print.huxtable <- function(x, ...) {
  meth <- getOption("huxtable.print", default = print_screen)
  if (is.character(meth)) meth <- eval(as.symbol(meth))

  meth(x, ...)
}


#' @rdname print.huxtable
#' @param output Output format. One of `"html"`, `"latex"`, `"md"`, `"screen"` or `"rtf"`.
#'
#' @return `format` returns a string representation from [to_latex()], [to_html()] etc.
#' @export
#'
#' @doctest
#' @expect snapshot(cat(.))
#' format(jams, output = "screen")
#' @expect snapshot(cat(.))
#' format(jams, output = "md")
format.huxtable <- function(x, ..., output = c("latex", "html", "md", "screen", "rtf")) {
  output <- match.arg(output)

  fn <- paste0("to_", output)
  do.call(fn, list(ht = x, ...))
}
