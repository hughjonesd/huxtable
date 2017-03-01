
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

#' @export
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
  res <- paste0('<table class="huxtable" style="width: ', width, '; ', mstring, heightstring, '">\n')
  if (! is.na(cap <- caption(ht))) {
    cap <- paste0('<caption style="caption-side:', caption_pos(ht),'; text-align: center;">', cap, '</caption>')
    res <- paste0(res, cap)
  }
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
  col_width <- col_width(ht)[cn]
  if (is.numeric(col_width)) col_width <- paste0(col_width * 100, '%')
  res <- paste0('<col style="width: ', col_width ,';">')
  # print out <col>, <colgroup>, that kinda stuff
  res
}

row_html <- function (ht, rn) {
  # print out <tr>, <td> or maybe <th> etc., then </tr>
  style <- ''
  if (! is.na(height <- row_height(ht)[rn])) {
    if (is.numeric(height)) height <- paste0(round(height * 100, 1), '%')
    style <- paste0(' style="height: ', height, ';"')
  }
  res <- paste0('<tr', style ,'>\n')
  cols_to_show <- 1:ncol(ht)
  display_cells <- display_cells(ht) # speedup: make this call just once in parent
  cols_to_show <- setdiff(cols_to_show, display_cells$col[display_cells$row == rn & display_cells$shadowed])
  cells_html <- sapply(cols_to_show, cell_html, ht = ht, rn = rn)
  cells_html <- paste0(cells_html, collapse = '')
  res <- paste0(res, cells_html)
  res <- paste0(res, '</tr>\n')
  res
}

cell_html <- function (ht, rn, cn) {
  res <- '  <td '
  rs <- rowspan(ht)[rn,cn]
  cs <- colspan(ht)[rn,cn]
  if (isTRUE(rs > 1)) res <- paste0(res, ' rowspan="', rs ,'"')
  if (isTRUE(cs > 1)) res <- paste0(res, ' colspan="', cs ,'"')

  res<- paste0(res, ' style="')
  val <- valign(ht)[rn, cn]
  res <- paste0(res, 'vertical-align: ', val, '; ')
  al  <- align(ht)[rn, cn]
  res <- paste0(res, 'text-align: ', al, '; ')


  borders <- c(top_border(ht)[rn, cn], right_border(ht)[rn, cn], bottom_border(ht)[rn, cn],
    left_border(ht)[rn, cn])
  if (any(borders > 0)) {
    borders <- paste(borders, 'px', sep = '', collapse = ' ')
    res <- paste0(res, 'border-width:', borders, '; ')
    res <- paste0(res, 'border-style: solid; ')
  }

  padding <- list(top_padding(ht)[rn, cn], right_padding(ht)[rn, cn], bottom_padding(ht)[rn, cn],
    left_padding(ht)[rn, cn])
  if (any( ! is.na(padding))) {
    padding <- sapply(padding, function(x) if(is.numeric(x)) paste0(x, "px") else x)
    padding <- paste(padding, collapse = ' ')
    res <- paste0(res, 'padding: ', padding, '; ')
  }

  if (! is.na(bgcolor <- background_color(ht)[rn, cn])) {
    bgcolor <- as.vector(col2rgb(bgcolor))
    bgcolor <- paste(bgcolor, collapse = ', ')
    res <- paste0(res, 'background-color: rgb(', bgcolor, '); ')
  }

  res <- paste0(res, '">')

  contents <- clean_contents(ht, rn, cn, type = 'html')

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

  if (! (span_css == '')) contents <- paste0('<span style="', span_css, '">', contents, '</span>')

  if ((rt <- rotation(ht)[rn, cn]) != 0) {
    # note the minus sign
    contents <- paste0('<div style="transform: rotate(-', rt,'deg); white-space:nowrap;">', contents, '</div>')
  }
  res <- paste0(res, contents)
  res <- paste0(res, '</td>\n')
  res
}


