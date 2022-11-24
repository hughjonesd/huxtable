
#' @import assertthat
NULL


#' @export
#' @rdname to_md
print_md <- function(ht, ...) cat(to_md(ht, ...))


#' Create Markdown representing a huxtable
#'
#' @param ht        A huxtable.
#' @param header    Logical. Print the first row as a header?
#' @param min_width Minimum width in on-screen characters of the result.
#' @param max_width Maximum width in on-screen characters of the result. Overrides `min_width`.
#' @param ...       Arguments passed to methods.
#'
#' @return `to_md()` returns a string. `print_md()` prints the string and returns
#' `NULL`.
#' @export
#'
#' @details
#' Only `align` and `caption` properties are used. The markdown format is
#' `multiline_tables`, see the \href{https://pandoc.org/MANUAL.html#pandocs-markdown}{pandoc documentation}.
#'
#' @family printing functions
#'
#' @doctest
#' @snap
#' print_md(jams)
to_md <- function(ht, ...) UseMethod("to_md")


#' @export
#' @rdname to_md
to_md.huxtable <- function(ht, header = TRUE, min_width = getOption("width") / 4, max_width = 80, ...) {
  assert_that(is.flag(header), is.number(min_width), is.number(max_width))
  if (! check_positive_dims(ht)) {
    return("")
  }
  if (any(colspan(ht) > 1 | rowspan(ht) > 1)) warning("Markdown cannot handle cells with colspan/rowspan > 1")
  align <- real_align(ht)
  if (any(apply(align, 2, function(x) length(unique(x)) > 1)))
    warning("Can't vary column alignment in markdown; using first row")
  ht <- map_align(ht, by_cols(align[1,]))

  charmat_data <- character_matrix(ht, inner_border_h = 1, outer_border_h = 1, inner_border_v = 1,
    outer_border_v = 1, min_width = min_width, max_width = max_width, markdown = TRUE)
  charmat <- charmat_data$charmat
  border_rows <- charmat_data$border_rows
  border_cols <- charmat_data$border_cols
  last_ht_col <- charmat_data$last_ht_col
  if (last_ht_col < ncol(ht)) warning(glue::glue(
    "Couldn't print whole table in max_width = {max_width} characters.\n",
    "Printing {last_ht_col}/{ncol(ht)} columns."
  ))
  # if you have a header, you need a whole line of dashes. Otherwise just to indicate columns
  dash_cols <- if (header) seq_len(ncol(charmat)) else - (border_cols)
  charmat[c(1, nrow(charmat)), dash_cols] <- "-"
  if (header) {
    omit_cols <- border_cols[ - c(1, ncol(ht) + 1)] # skip interior borders
    charmat[border_rows[2], - omit_cols ] <- "-"
  }

  result <- paste(apply(charmat, 1, paste0, collapse = ""), collapse = "\n")
  result <- paste0(result, "\n\n")
  if (! is.na(cap <- make_caption(ht, make_label(ht), "md"))) {
    result <- paste0(result, "Table: ", cap, "\n")
  }

  result
}
