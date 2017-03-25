

#' @export
#'
#' @rdname to_html
#'
print_html <- function(ht, ...) cat(to_html(ht, ...))


#' Create HTML representing a huxtable
#'
#' These functions print or return an HTML table.
#'
#' @param ht A huxtable.
#' @param ... Arguments to pass to methods. Not currently used.
#'
#' @return \code{to_html} returns an HTML string. \code{print_html} prints the string and returns \code{NULL}.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_html(ht)
to_html <- function (ht, ...) UseMethod('to_html')


#' @export
#'
#' @rdname to_html
#'
#' @return \code{print_notebook} prints HTML output suitable for use in an
#' RStudio interactive notebook.
print_notebook <- function(ht, ...) print(rmarkdown::html_notebook_output_html(to_html(ht)))



#' @export
#' @rdname to_html
to_html.huxtable <- function(ht, ...) {
  width <- width(ht)
  if (is.numeric(width)) width <- paste0(width * 100, '%')
  mstring <- switch(position(ht),
          left   = 'margin-left: 0%;',
          right  = 'margin-right: 0%;',
          center = 'margin-left: auto; margin-right: auto;'
        )
  heightstring <- ''
  if (! is.na(height <- height(ht))) {
    if (is.numeric(height)) height <- paste0(height * 100, '%')
    heightstring <- paste0('height: ', height, ';')
  }
  idstring <- ''
  if (! is.na(label <- label(ht))) idstring <- paste0(' id="', label, '"')
  res <- paste0('<table class="huxtable" style="border-collapse: collapse; width: ', width, '; ', mstring,
        heightstring, '"', idstring,'>\n')
  if (! is.na(cap <- caption(ht))) {
    cap <- paste0('<caption style="caption-side:', caption_pos(ht), '; text-align: center;">', cap, '</caption>')
    res <- paste0(res, cap)
  }
  cols_html <- sapply(1:ncol(ht), col_html, ht = ht)
  cols_html <- paste0(cols_html, collapse = '')
  res <- paste0(res, cols_html)
  contents <- clean_contents(ht, type = 'html')
  rows_html <- sapply(1:nrow(ht), row_html, ht = ht, contents)
  rows_html <- paste0(rows_html, collapse = '')
  res <- paste0(res, rows_html)
  res <- paste0(res, '</table>\n')
  res
}


col_html <- function (ht, cn) {
  col_width <- col_width(ht)[cn]
  if (is.numeric(col_width)) col_width <- paste0(col_width * 100, '%')
  res <- paste0('<col style="width: ', col_width, ';">')
  # print out <col>, <colgroup>, that kinda stuff
  res
}


row_html <- function (ht, rn, contents) {
  # print out <tr>, <td> or maybe <th> etc., then </tr>
  style <- ''
  if (! is.na(height <- row_height(ht)[rn])) {
    if (is.numeric(height)) {
      height <- height / sum(row_height(ht), na.omit = TRUE)
      height <- paste0(round(height * 100, 1), '%')
    }
    style <- paste0(' style="height: ', height, ';"')
  }
  res <- paste0('<tr', style, '>\n')
  cols_to_show <- 1:ncol(ht)
  dcells <- display_cells(ht, all = TRUE) # speedup: make this call just once in parent
  cols_to_show <- setdiff(cols_to_show, dcells$col[dcells$row == rn & dcells$shadowed])
  cells_html <- sapply(cols_to_show, cell_html, ht = ht, rn = rn, contents)
  cells_html <- paste0(cells_html, collapse = '')
  res <- paste0(res, cells_html)
  res <- paste0(res, '</tr>\n')
  res
}

cell_html <- function (ht, rn, cn, contents) {
  res <- '  <td '
  rs <- rowspan(ht)[rn, cn]
  cs <- colspan(ht)[rn, cn]
  if (isTRUE(rs > 1)) res <- paste0(res, ' rowspan="', rs, '"')
  if (isTRUE(cs > 1)) res <- paste0(res, ' colspan="', cs, '"')

  res <- paste0(res, ' style="')
  val <- valign(ht)[rn, cn]
  res <- paste0(res, 'vertical-align: ', val, '; ')
  al  <- align(ht)[rn, cn]
  res <- paste0(res, 'text-align: ', al, '; ')
  wrap <- wrap(ht)[rn, cn]
  res <- paste0(res, 'white-space: ', if (wrap) 'normal' else 'nowrap', '; ')

  borders <- get_all_borders(ht, rn, cn)
  borders <- borders[c('top', 'right', 'bottom', 'left')]
  borders <- paste(borders, 'px', sep = '', collapse = ' ')
  res <- paste0(res, 'border-width:', borders, '; ')
  res <- paste0(res, 'border-style: solid; ')

  bcols <- get_all_border_colors(ht, rn, cn)
  bcols <- bcols[c('top', 'right', 'bottom', 'left')]
  bcols <- na.omit(bcols)
  bcols <- if (length(bcols)) paste0('border-', names(bcols), '-color: ', bcols, '; ', collapse = ' ') else ''
  res <- paste0(res, bcols)

  padding <- list(top_padding(ht)[rn, cn], right_padding(ht)[rn, cn], bottom_padding(ht)[rn, cn],
    left_padding(ht)[rn, cn])
  if (any( ! is.na(padding))) {
    padding <- sapply(padding, function(x) if(is_a_number(x)) paste0(x, "pt") else x)
    padding <- paste(padding, collapse = ' ')
    res <- paste0(res, 'padding: ', padding, '; ')
  }

  if (! is.na(bgcolor <- background_color(ht)[rn, cn])) {
    bgcolor <- as.vector(col2rgb(bgcolor))
    bgcolor <- paste(bgcolor, collapse = ', ')
    res <- paste0(res, 'background-color: rgb(', bgcolor, '); ')
  }

  res <- paste0(res, '">')

  cell_contents <- contents[rn, cn]

  span_css <- ''
  if (! is.na(text_color <- text_color(ht)[rn, cn])) {
    text_color <- as.vector(col2rgb(text_color))
    text_color <- paste(text_color, collapse = ', ')
    # use span not td style because color affects borders
    span_css <- paste0(span_css, 'color: rgb(', text_color, '); ')
  }
  if (! is.na(font_size <- font_size(ht)[rn, cn])) {
    if (is.numeric(font_size)) font_size <- paste0(font_size, 'pt')
    span_css <- paste0(span_css, 'font-size:', font_size, '; ')
  }
  if (bold(ht)[rn, cn]) {
    span_css <- paste0(span_css, 'font-weight: bold; ')
  }
  if (italic(ht)[rn, cn]) {
    span_css <- paste0(span_css, 'font-style: italic; ')
  }
  if (! is.na(font <- font(ht)[rn, cn])) {
    span_css <- paste0(span_css, 'font-family: ', font, '; ')
  }

  if (! (span_css == '')) cell_contents <- paste0('<span style="', span_css, '">', cell_contents, '</span>')

  if ( (rt <- rotation(ht)[rn, cn]) != 0) {
    # note the minus sign
    cell_contents <- paste0('<div style="transform: rotate(-', rt, 'deg); white-space:nowrap;">', cell_contents,
          '</div>')
  }
  res <- paste0(res, cell_contents)
  res <- paste0(res, '</td>\n')
  res
}
