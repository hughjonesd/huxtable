

#' Convert a Huxtable to a FlexTable for Word/Powerpoint
#'
#' @param x A huxtable.
#' @param header_rows How many rows to use as headers.
#' @param footer_rows How many rows to use as footers.
#' @param ... Not currently used.
#'
#' @return an object of class FlexTable.
#'
#' @details
#'
#' Huxtable objects can be converted to \code{\link[ReporteRs]{FlexTable}} objects for use in Word documents.
#'
#' Properties are supported, with the following exceptions:
#' \itemize{
#'   \item Cells can span only multiple rows or multiple columns, but not both.
#'   \item Rotation of 0, 90 or 270 is supported, only in header rows.
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
#' if (require('ReporteRs')) {
#'   ht <- huxtable(a = 1:3, b = 1:3)
#'   ft <- as.FlexTable(ht)
#'   ft
#'   my_doc <- docx()
#'   my_doc <- addFlexTable(my_doc, ft)
#' }
#' @importFrom ReporteRs as.FlexTable
#' @method as.FlexTable huxtable
#' @aliases as.FlexTable
#' @export
#'
as.FlexTable.huxtable <- function(x, header_rows = 1, footer_rows = 0, ...) {
  if (! requireNamespace('ReporteRs')) stop('as.FlexTable requires the ReporteRs package. To install, type:\n',
    'install.packages("ReporteRs")')

  if (header_rows < 1) stop('Must have at least one header row')
  header_rows <- seq_len(header_rows)
  footer_rows <- seq_len(footer_rows) + nrow(x) - footer_rows

  dcells <- display_cells(x)
  dcells <- dcells[ ! dcells$shadowed, ]
  # rot <- switch(as.character(rotation(x)[drow, dcol]),
  #         '0'   = 'lrtb',
  #         '90'  = 'btlr',
  #         '270' = 'tbrl',
  #         stop_here("FlexTable can only handle rotation of 0, 90 or 270")
  #       )
  # cell_props$text.direction <- rot
  ft <- ReporteRs::FlexTable(numrow = nrow(x), numcol = ncol(x), header.columns = FALSE)
  for (hr in c(header_rows, footer_rows)) {
    contents <- apply(dcells[dcells$display_row == hr, c('display_row', 'display_col')], 1, function (y) {
      clean_contents(x, y[1], y[2], type = 'word')
    })
    colspans <- dcells$colspan[dcells$display_row == hr]

    func <- if (hr %in% header_rows) ReporteRs::addHeaderRow else ReporteRs::addFooterRow
    ft <- func(ft, value = contents, colspan = colspans)
  }

  for (j in 1:nrow(dcells)) {
    drow <- dcells$display_row[j]
    part <- if (drow %in% header_rows) 'header' else
          if (drow %in% footer_rows) 'footer' else 'body'
    target_row <- if (part == 'header') drow else if (part == 'footer') drow - min(footer_rows) + 1 else
          drow - max(header_rows)
    ft <- add_cell(ft, x, dcells[j,], part, target_row)
  }

  w <- width(x)
  # rough guess at page width of 6 inches:
  if (is.numeric(w)) w <- w * 6 else warning('FlexTable can only deal with numeric width, ignoring width of: ', w)
  if (! any(is.na(cw <- col_width(x)))) {
    if (is.numeric(cw)) ft <- ReporteRs::setFlexTableWidths(ft, cw * w) else
          warning('FlexTable can only deal with numeric col_width, ignoring col_width of: ', paste(cw, collapse=', '))
  }

  ft
}

add_cell <- function(ft, ht, dc, part, target_row) {
  drow <- dc$display_row
  dcol <- dc$display_col

  stop_here <- function(text) stop(text, ' in cell (', drow, ', ', dcol, ')')

  force(ft)
  add_stuff <- function(value) ft[target_row, dcol, to = part] <<- value
  if (part == 'body') add_stuff(clean_contents(ht, drow, dcol, 'word'))

  text_props <- list(
          font.weight = if (bold(ht)[drow, dcol]) 'bold' else 'normal',
          font.style  = if (italic(ht)[drow, dcol]) 'italic' else 'normal'
        )
  if (! is.na(text_color <- text_color(ht)[drow, dcol])) text_props$color <- text_color
  if (! is.na(font_size <- font_size(ht)[drow, dcol])) text_props$font.size <- font_size
  if (! is.na(font <- font(ht)[drow, dcol])) text_props$font.family <- font
  tp_obj <- do.call(ReporteRs::textProperties, text_props)
  add_stuff(tp_obj)

  pad <- c(l = left_padding(ht)[drow, dcol], t = top_padding(ht)[drow, dcol], r = right_padding(ht)[drow, dcol],
        b = bottom_padding(ht)[drow, dcol])
  if (! is.numeric(pad)) stop_here("FlexTable cannot handle non-numeric value for padding")
  cell_props <- list(
          padding.bottom = pad['b'],
          padding.left   = pad['l'],
          padding.top    = pad['t'],
          padding.right  = pad['r'],
          border.style   = 'solid',
          border.color   = 'black',
          border.bottom.width = bottom_border(ht)[drow, dcol],
          border.left.width   = left_border(ht)[drow, dcol],
          border.top.width    = top_border(ht)[drow, dcol],
          border.right.width  = right_border(ht)[drow, dcol],
          vertical.align      = valign(ht)[drow, dcol]
  )
  if (! is.na(bgc <- background_color(ht)[drow, dcol])) cell_props$background.color <- bgc
  rot <- switch(as.character(rotation(ht)[drow, dcol]),
          '0'   = 'lrtb',
          '90'  = 'btlr',
          '270' = 'tbrl',
          NA
        )
  if (is.na(rot)) warning('FlexTable can only handle rotation of 0, 90 or 270')
  if (rot != 'lrtb' && part != 'header') warning('FlexTable can only set rotation of header rows')
  cell_props$text.direction <- rot
  cellp_obj <- do.call(ReporteRs::cellProperties, cell_props)
  add_stuff(cellp_obj)
  add_stuff(ReporteRs::parProperties(text.align = align(ht)[drow, dcol]))

  if (part == 'body') {
    cs <- dc$colspan
    rs <- dc$rowspan
    if (cs > 1 & rs > 1) stop_here('FlexTable cannot span both rows and columns')
    if (cs > 1) ft <- ReporteRs::spanFlexTableColumns(ft, drow, dcol, dc$end_col)
    if (rs > 1) ft <- ReporteRs::spanFlexTableRows(ft, dcol, drow, dc$end_row)
  }

  ft
}
