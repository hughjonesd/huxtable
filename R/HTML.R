
#' Create HTML Representing a Huxtable
#'
#' @param ht A huxtable.
#' @param ...
#'
#' @return \code{to_html} returns an HTML string. \code{print_html} prints the string and returns \code{NULL}.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_html(ht)
to_html <- function (ht, ...) UseMethod('to_html')

to_html.huxtable <- function(ht, ...) {
  width <- width(ht)
  if (is.numeric(width)) width <- paste0(width * 100, '%')
  res <- paste0('<table class="huxtable" style="width: ', width, ';">\n')
  cols_html <- sapply(1:ncol(ht), col_html, ht = ht)
  cols_html <- paste0(cols_html, collapse = '')
  res <- paste0(res, cols_html)
  rows_html <- sapply(1:nrow(ht), row_html, ht = ht)
  rows_html <- paste0(rows_html, collapse = '')
  res <- paste0(res, rows_html)
  res <- paste0(res, '</table>\n')
  res
}

#' @export
#'
#' @rdname to_html
print_html <- function(ht, ...) {
  cat(to_html(ht, ...))
}

col_html <- function (ht, cn) {
  col_width <- col_widths(ht)[cn]
  res <- paste0('<col style="width: ', col_width ,';">')
  # print out <col>, <colgroup>, that kinda stuff
  res
}

row_html <- function (ht, rn) {
  # print out <tr>, <td> or maybe <th> etc., then </tr>
  res <- '<tr class="huxtable-row">\n'
  cells_html <- sapply(1:ncol(ht), cell_html, ht = ht, rn = rn)
  cells_html <- paste0(cells_html, collapse = '')
  res <- paste0(res, cells_html)
  res <- paste0(res, '</tr>\n')
  res
}

cell_html <- function (ht, rn, cn) {
  # check if cell is shadowed by an earlier row/colspan.
  # we could reduce repetition a lot by doing this calculation once per call!
  if (cell_shadowed(ht, rn, cn, 'row')) return('')
  if (cell_shadowed(ht, rn, cn, 'col')) return('')
  res <- '  <td class="huxtable-cell"'
  rs <- rowspan(ht)[rn,cn]
  cs <- colspan(ht)[rn,cn]
  if (isTRUE(rs > 1)) res <- paste0(res, ' rowspan="', rs ,'"')
  if (isTRUE(cs > 1)) res <- paste0(res, ' colspan="', cs ,'"')
  res<- paste0(res, ' style="')
  val <- valign(ht)[rn, cn]
  res <- paste0(res, 'vertical-align: ', val, '; ')
  al  <- align(ht)[rn, cn]
  res <- paste0(res, 'text-align: ', al, '; ')
  if (! is.na(bgcolor <- bgcolor(ht)[rn, cn])) {
    bgcolor <- as.vector(col2rgb(bgcolor))
    bgcolor <- paste(bgcolor, collapse = ', ')
    res <- paste0(res, 'background-color: rgb(', bgcolor, '); ')
  }
  res <- paste0(res, '">')
  res <- paste0(res, ht[rn, cn]) # eventually should be formatted!
  res <- paste0(res, '</td>\n')
  res
}


