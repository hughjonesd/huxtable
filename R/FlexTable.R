


#' Convert a Huxtable for Word/Powerpoint
#'
#' Huxtables can be converted to \code{\link[ReporteRs]{FlexTable}} objects, for use in Word and Powerpoint documents.
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
#' Note: you can't use ReporteRs Word output with R markdown word output. Instead you have to write the Word file
#' yourself. See the ReporteRs documentation.
#'
#' Properties are supported, with the following exceptions:
#' \itemize{
#'   \item Cells can span only multiple rows or multiple columns, but not both.
#'   \item Rotation of 0, 90 or 270 is supported, only in header rows.
#'   \item Non-numeric column widths and table widths are not supported. Table width is treated as a fraction of 6
#'     inches.
#'   \item Table height, row heights, wrap, captions and table position are not supported.
#' }
#'
#' @section Challenge:
#'
#' Try to say \code{as_FlexTable.huxtable} ten times without pausing.
#'
#' @examples
#' if (require('ReporteRs')) {
#'   ht <- huxtable(a = 1:3, b = 1:3)
#'   ft <- as_FlexTable(ht)
#'   ft
#'   my_doc <- docx()
#'   my_doc <- addFlexTable(my_doc, ft)
#' }
#' @aliases as_FlexTable.huxtable
#' @export
#'
as_FlexTable <- function(x, ...) UseMethod('as_FlexTable')


#' @rdname as_FlexTable
#' @export
as_FlexTable.huxtable <- function(x, header_rows = 0, footer_rows = 0, ...) {
  if (! requireNamespace('ReporteRs')) stop('as.FlexTable requires the ReporteRs package. To install, type:\n',
    'install.packages("ReporteRs")')

  ht <- x
  hrows <- seq_len(header_rows)
  frows <- seq_len(footer_rows) + nrow(ht) - footer_rows

  dcells <- display_cells(ht)
  dcells <- dcells[ ! dcells$shadowed, ]

  ft <- ReporteRs::FlexTable(numrow = nrow(ht) - header_rows - footer_rows, numcol = ncol(ht), header.columns = FALSE)

  contents <- clean_contents(ht, type = 'word')
  for (hr in c(hrows, frows)) {
    cell_contents <- apply(dcells[dcells$display_row == hr, c('display_row', 'display_col')], 1, function (y) {
      contents[y[1], y[2]]
    })
    colspans <- dcells$colspan[dcells$display_row == hr]
    cell_props <- if (! is.na(td <- get_text_dir(ht, hr))) cellProperties(text.direction = td) else cellProperties()
    func <- if (hr %in% hrows) ReporteRs::addHeaderRow else ReporteRs::addFooterRow
    ft <- func(ft, value = cell_contents, colspan = colspans, cell.properties = cell_props)
  }

  for (j in 1:nrow(dcells)) {
    drow <- dcells$display_row[j]
    dcol <- dcells$display_col[j]
    part <- if (drow %in% hrows) 'header' else if (drow %in% frows) 'footer' else 'body'
    if (part == 'body') ft[drow - header_rows, dcol, to = part] <- contents[drow, dcol]
    ft <- format_cell(ft, ht, dcells[j,], part, header_rows, footer_rows)
  }

  w <- width(ht)
  # rough guess at page width of 6 inches:
  if (is.numeric(w)) w <- w * 6 else warning('FlexTable can only deal with numeric width, ignoring width of: ', w)
  if (! any(is.na(cw <- col_width(ht)))) {
    if (is.numeric(cw)) ft <- ReporteRs::setFlexTableWidths(ft, cw * w) else
          warning('FlexTable can only deal with numeric col_width, ignoring col_width of: ', paste(cw, collapse=', '))
  }

  ft
}


format_cell <- function(ft, ht, dc, part, header_rows, footer_rows) {
  drow <- dc$display_row
  dcol <- dc$display_col

  stop_here <- function(text) stop(text, ' in cell (', drow, ', ', dcol, ')')

  target_row <- switch(part,
          header = drow,
          body   = drow - header_rows,
          footer = drow - nrow(ht) + footer_rows
        )
  force(ft)
  add_stuff <- function(value) ft[target_row, dcol, to = part] <<- value

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

  # both this and the setHeaderRow call in the parent are necessary:
  if (! is.na(rot <- get_text_dir(ht, drow))) {
    cell_props$text.direction <- rot
    if (part == 'body') warning('Cell rotation only works in header and footer cells')
  }
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

get_text_dir <- function (ht, row) {
  rot <- rotation(ht)[row,]
  if (length(unique(rot)) > 1) warning('FlexTable cannot handle multiple rotation values per row')
  rot <- rot[1]
  rot <- switch(as.character(rot),
    '0'   = NA,
    '90'  = 'btlr',
    '270' = 'tbrl',
    {warning('FlexTable can only handle rotation of 0, 90 or 270'); NA}
  )

  rot
}
