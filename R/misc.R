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
#' jams <- add_footnote(
#'   jams,
#'   "* subject to availability"
#' )
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
  if (!is.null(border)) ht <- set_top_border(ht, nr, everywhere, border)
  wrap(ht)[nr, 1] <- TRUE
  if (!missing(...)) ht <- set_cell_properties(ht, nr, 1, ...)
  ht <- set_number_format(ht, nr, 1, number_format)

  ht
}



#' Escape text for various formats
#'
#' This escapes a string for LaTeX, HTML, Typst or RTF.
#'
#' @param str A character object.
#' @param type `"latex"`, `"html"`, `"typst"` or `"rtf"`.
#'
#' @return The sanitized character object.
#'
#' @details
#' HTML and LaTeX code was copied over from `xtable::sanitize()`.
#'
#' @export
#'
#' @examples
#' txt <- "Make $$$ with us"
#' sanitize(txt, type = "latex")
sanitize <- function(str, type = c("latex", "html", "typst", "rtf")) {
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
      result,
      fixed = TRUE
    )
  } else if (type == "html") {
    result <- gsub("&", "&amp;", result, fixed = TRUE)
    result <- gsub(">", "&gt;", result, fixed = TRUE)
    result <- gsub("<", "&lt;", result, fixed = TRUE)
    result <- gsub("\n", "<br>", result, fixed = TRUE)
  } else if (type == "rtf") {
    result <- gsub("\\", "\\\\", result, fixed = TRUE)
    result <- gsub("{", "\\{", result, fixed = TRUE)
    result <- gsub("}", "\\}", result, fixed = TRUE)
    result <- gsub("\n", "\\line ", result, fixed = TRUE)
  } else if (type == "typst") {
    # There is a reason why this one has to come first!!!
    result <- gsub("\\", "\\\\", result, fixed = TRUE)
    to_escape <- c("#", "[", "]", "*", "$", "_", "`", "<", ">", "=", "@", "-", "+", "/")
    for (char in to_escape) {
      escaped <- paste0("\\", char)
      result <- gsub(char, escaped, result, fixed = TRUE)
    }
  }

  return(result)
}


#' Huxtable logo
#'
#' Returns a randomized huxtable logo, inspired by Mondrian.
#'
#' @param compact Logical. Create a compact 1-row huxtable (default is 2 rows)?
#' @param latex Logical. Output for LaTeX?
#'
#' @return A huxtable.
#' @export
#'
#' @examples
#' # Default logo
#' print_screen(hux_logo())
#'
#' # Compact single-row version
#' print_screen(hux_logo(compact = TRUE))
#'
hux_logo <- function(compact = FALSE, latex = NULL) {
  assert_that(is.flag(compact))
  assert_that(is.null(latex) || is.flag(latex))

  # Auto-detect LaTeX if not specified
  if (is.null(latex)) {
    latex <- guess_knitr_output_format() == "latex"
  }

  # Create logo layout
  hux_letters <- c("h", "u", "X", "t", "a", "b", "l", "e")
  if (compact) {
    logo <- as_hux(matrix(hux_letters, nrow = 1), add_colnames = FALSE)
  } else {
    logo <- as_hux(rbind(hux_letters, hux_letters), add_colnames = FALSE)
    logo[1, 4:8] <- ""
    logo[2, 1:2] <- ""
    rowspan(logo)[1, 3] <- 2
    colspan(logo)[1, 5] <- 3
  }

  n_cells <- nrow(logo) * ncol(logo)

  # Set background colors
  background_color(logo) <- "white"
  n_color_cells <- if (compact) 2 else 3
  selected_cells <- sample(n_cells, n_color_cells)
  special_colors <- sample(c("white", "blue", "yellow", "red"),
                          n_color_cells, replace = TRUE,
                          prob = c(0.1, 0.4, 0.4, 0.1))
  background_color(logo)[selected_cells] <- special_colors
  background_color(logo)[1, 3] <- "red"  # X always red

  # Set fonts
  serif_font <- if (latex) "cmr" else "Times"
  sans_font <- if (latex) "cmss" else "Arial"
  font_choices <- sample(c(serif_font, sans_font), n_cells, replace = TRUE)
  font(logo) <- matrix(font_choices, nrow = nrow(logo))
  font(logo)[1, 3] <- serif_font  # X always serif

  # Set bold
  bold_choices <- sample(c(FALSE, TRUE), n_cells, replace = TRUE, prob = c(0.8, 0.2))
  bold(logo) <- matrix(bold_choices, nrow = nrow(logo))
  bold(logo)[1, 3] <- FALSE  # X never bold

  italic(logo) <- sample(c(FALSE, TRUE), n_cells, replace = TRUE, prob = c(0.8, 0.2))
  italic(logo)[1, 3] <- FALSE # X never italic

  # Set other styling
  text_color(logo) <- "black"
  border_width <- if (compact) 0.4 else 1.2
  logo <- set_all_borders(logo, border_width)
  logo <- set_all_border_colors(logo, "black")
  logo <- set_all_padding(logo, if (latex) 2 else 4)
  align(logo) <- "centre"
  valign(logo) <- "middle"
  font_size(logo) <- 14
  if (!compact) font_size(logo)[1, 3] <- 24  # X larger in non-compact

  logo
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
#' # to print LaTeX output:
#' options(huxtable.print = print_latex)
#' # to print Typst output:
#' options(huxtable.print = print_typst)
#' }
print.huxtable <- function(x, ...) {
  meth <- getOption("huxtable.print", default = print_screen)
  if (is.character(meth)) meth <- eval(as.symbol(meth))

  meth(x, ...)
}


#' @rdname print.huxtable
#' @param output Output format. One of `"html"`, `"latex"`, `"md"`, `"screen"`, `"rtf"` or `"typst"`.
#'
#' @return `format` returns a string representation from [to_latex()], [to_html()] etc.
#' @export
#'
#' @examples
#'
#' format(jams, output = "screen")
#' format(jams, output = "md")
#' format(jams, output = "typst")
format.huxtable <- function(x, ..., output = c("latex", "html", "md", "screen", "rtf", "typst")) {
  output <- match.arg(output)

  fn <- paste0("to_", output)
  do.call(fn, list(ht = x, ...))
}
