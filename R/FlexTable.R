

#' Convert a Huxtable to a FlexTable for Word/Powerpoint
#'
#' @param x a huxtable.
#' @param header_rows Which rows to use as headers. Must be only the first n rows.
#' @param footer_rows Which rows to use as footers. Must be only the last n rows.
#' @param ... Not currently used.
#'
#' @return an object of class FlexTable.
#' @export
#'
#' @details
#'
#' Huxtable objects can be converted to \code{\link[ReporteRs]{FlexTable}} objects for use in Word documents.
#'
#' Properties are supported, with the following exceptions:
#' \itemize{
#'   \item Cells can span only multiple rows or multiple columns, but not both.
#'   \item Rotation of 0, 90 or 270 is supported.
#'   \item Non-numeric column widths and table widths are not supported. Table width is treated as a fraction of 6
#'     inches.
#'   \item Table height, row heights, captions and table position are not supported.
#' }
#'
#' @section Challenge:
#'
#' Try to say \code{as.FlexTable.huxtable} ten times without pausing.
#'
#' @examples
#' if (requireNameSpace('ReporteRs')) {
#'   ht <- huxtable(a = 1:3, b = 1:3)
#'   ft <- as.FlexTable(ht)
#'   my_doc <- docx()
#'   my_doc <- addFlexTable(my_doc, ft)
#' }
#' @importFrom ReporteRs as.FlexTable
as.FlexTable.huxtable <- function(x, header_rows = 1, footer_rows = numeric(0), ...) {
  if (! requireNamespace('ReporteRs')) stop('as.FlexTable requires the ReporteRs package. To install, type:\n',
    'install.packages("ReporteRs")')

  if (is.null(header_rows)) header_rows <- numeric(0)
  if (is.null(footer_rows)) footer_rows <- numeric(0)
  if (! all(sort(header_rows) == seq_len(header_rows))) stop('Only first n rows can be headers')
  if (length(footer_rows) && ! all(sort(footer_rows) == seq(min(footer_rows), nrow(x), 1))) stop(
        'Only last n rows can be footers')

  dcells <- display_cells(x)
  dcells <- dcells[ !duplicated(dcells[,c('display_row', 'display_col')]) ]
  ft <- FlexTable(numrow = nrow(x), numcol = ncol(x), header.columns = FALSE)
  for (j in 1:nrow(dcells)) {
    ft <- add_cell(ft, x, dcells[j,], j, header_rows, footer_rows)
  }
  w <- width(ht)
  # rough guess at page width of 6 inches:
  if (is.numeric(w)) w <- w * 6 else warning('FlexTable can only deal with numeric width, ignoring width of: ', w)
  if (! any(is.na(cw <- col_width(ht)))) {
    if (is.numeric(cw)) ft <- setFlexTableWidths(ft, cw * w) else
          warning('FlexTable can only deal with numeric col_width, ignoring col_width of: ', paste(cw, collapse=', '))
  }

  ft
}

add_cell <- function(ft, ht, dc, j, header_rows, footer_rows) {
  drow <- dc$display_row
  dcol <- dc$display_col

  stop_here <- function(text) stop(text, ' in cell (', drow, ', ', dcol, ')')

  part <- if (j %in% header_rows) 'header' else if (j %in% footer_rows) 'footer' else 'body'
  force(ft)
  target_row <- if (j %in% header_rows) j else if (j %in% footer_rows) j - min(footer_rows) + 1 else j - max(header_rows)
  add_stuff <- function(value) ft[target_row, dcol, to = part] <<- value
  add_stuff(clean_contents(ht, drow, dcol, 'word'))


  text_props <- list(
          font.weight = if (bold(ht)[drow, dcol]) 'bold' else 'normal',
          font.style  = if (italic(ht)[drow, dcol]) 'italic' else 'normal'
        )
  if (! is.na(text_color <- text_color(ht)[drow, dcol])) text_props$color <- text_color
  if (! is.na(font_size <- font_size(ht)[drow, dcol])) text_props$font.size <- font_size
  if (! is.na(font <- font(ht)[drow, dcol])) text_props$font.family <- font
  tp_obj <- do.call(textProperties, text_props)
  add_stuff(tp_obj)

  pad <- c(l = left_padding(ht)[drow, dcol], t = top_padding(ht)[drow, dcol], r = right_padding(ht)[drow, dcol],
        b = bottom_padding(ht)[drow, dcol])
  if (! is.numeric(pad)) stop_here("FlexTable cannot handle non-numeric value for padding")
  cell_props <- list(
          padding.bottom = pad$b,
          padding.left   = pad$l,
          padding.top    = pad$t,
          padding.right  = pad$r,
          border.style   = 'solid',
          border.color   = 'black',
          border.bottom.width = bottom_border(ht)[drow, dcol],
          border.left.width   = left_border(ht)[drow, dcol],
          border.top.width    = top_border(ht)[drow, dcol],
          border.right.width  = right_border(ht)[drow, dcol],
          vertical.align      = if ((va <- valign(ht)[drow, dcol]) == 'middle') 'center' else va
  )
  if (! is.na(bgc <- background_color(ht)[drow, dcol])) cell_props$background.color <- bgc
  rot <- switch(as.character(rotation(ht)[drow, dcol]),
          '0'   = 'lrtb',
          '90'  = 'btlr',
          '270' = 'tbrl',
          stop_here("FlexTable can only handle rotation of 0, 90 or 270")
        )
  cell_props$text.direction <- rot
  cellp_obj <- do.call(cellProperties, cell_props)
  add_stuff(cellp_obj)
  add_stuff(parProperties(text.align = align(ht)[drow, dcol]))

  cs <- dc$colspan
  rs <- dc$rowspan
  if (cs > 1 & rs > 1) stophere('FlexTable cannot span both rows and columns')
  if (cs > 1) ft <- spanFlexTableColumns(ft, drow, dcol, dc$end_col)
  if (rs > 1) ft <- spanFlexTableRows(ft, dcol, drow, dc$end_row)

  ft
}
