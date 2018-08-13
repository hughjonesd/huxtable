
#' @import assertthat
NULL


#' Convert a huxtable for Excel
#'
#' If the `openxlsx` package is installed, Huxtables can be converted to
#' [openxlsx::openxlsx()] Worbook objects, for use in Excel documents.
#'
#' @param ht A huxtable.
#' @param Workbook An existing `Workbook` object. By default, a new workbook will be created.
#' @param sheet Name for the worksheet where the huxtable will be created.
#' @param write_caption If `TRUE`, print any caption in the row above or below the table.
#' @param ... Not used.
#'
#' @details
#' Use [openxlsx::saveWorkbook()] to save the resulting object to an Excel file.
#'
#' Properties are supported with the following exceptions:
#' * Non-numeric column widths and row heights, table width and height.
#' * Decimal padding.
#' * Cell padding.
#' * Table position.
#'
#' Huxtable tries to guess appropriate widths and height for rows and columns; numeric [width()] and
#' [height()] are treated as scaling factors.
#'
#' Contents are only stored as numbers if a whole column is numeric as defined by [is_a_number()];
#' otherwise they are stored as text.
#'
#' @return An object of class `Workbook`.
#' @export
#'
#' @examples
#' ht <- hux(a = 1:3, b = 1:3)
#' wb <- as_Workbook(ht)
#'
#' \dontrun{
#' openxlsx::saveWorkbook(wb, "my-excel-file.xlsx")
#' }
#'
#' # multiple sheets in a single workbook:
#' wb <- openxlsx::createWorkbook()
#' wb <- as_Workbook(ht, Workbook = wb, sheet = 'sheet1')
#' wb <- as_Workbook(hux('Another', 'huxtable'), Workbook = wb, sheet = 'sheet2')
as_Workbook <- function (ht, ...) UseMethod('as_Workbook')


#' @export
#' @rdname as_Workbook
as_Workbook.huxtable <- function (ht,  Workbook = NULL, sheet = "Sheet 1", write_caption = TRUE, ...) {
  assert_package('as_Workbook', 'openxlsx')
  assert_that(is.string(sheet))

  wb <- if (missing(Workbook) || is.null(Workbook)) openxlsx::createWorkbook() else Workbook
  openxlsx::addWorksheet(wb, sheet)

  cap <- caption(ht)
  cap_pos <- caption_pos(ht)
  top_cap <- write_caption && ! is.na(cap) && grepl("top", cap_pos)
  cap_row <- if (top_cap) 1 else nrow(ht) + 1
  if (write_caption && ! is.na(cap)) {
    openxlsx::writeData(wb, sheet, x = cap, startRow = cap_row)
    cap_style <- openxlsx::createStyle(halign = get_caption_hpos(ht))
    openxlsx::addStyle(wb, sheet, style = cap_style, rows = cap_row, cols = seq_len(ncol(ht)),
      gridExpand = TRUE)
    openxlsx::mergeCells(wb, sheet, cols = seq_len(ncol(ht)), rows = cap_row)
  }

  contents <- clean_contents(ht, type = 'excel') # character matrix

  nr <- nrow(contents)
  contents <- as.data.frame(contents, stringsAsFactors = FALSE)
  is_a_number_mx <- is_a_number(contents)
  # for each column we go down it. If everything remaining is one type, we insert it. Otherwise
  # we insert the cell.
  for (j in seq_len(ncol(contents))) {
    col_contents <- contents[[j]]
    for (i in seq_len(nr)) {
      is_a_number_col <- is_a_number_mx[i:nr, j]
      if (all(is_a_number_col) || all(! is_a_number_col)) {
        insert <- col_contents[i:nr]
        if (all(is_a_number_col)) insert <- as.numeric(insert)
        openxlsx::writeData(wb, sheet, insert, startRow = 1 * top_cap + i, startCol = j,
              colNames = FALSE, rowNames = FALSE, borders = 'none', borderStyle = 'none')
        break # to the next column
      } else {
        insert <- col_contents[i]
        if (is_a_number_col[1]) insert <- as.numeric(insert)
        openxlsx::writeData(wb, sheet, insert, startRow = 1 * top_cap + i, startCol = j,
            colNames = FALSE, rowNames = FALSE, borders = 'none', borderStyle = 'none')
      }
    }
  }

  dcells <- display_cells(ht, all = FALSE)
  for (r in seq_len(nrow(dcells))) {
    dcell <- dcells[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col

    workbook_rows <- seq(drow, dcell$end_row)
    workbook_cols <- seq(dcol, dcell$end_col)
    if (top_cap) workbook_rows <- workbook_rows + 1

    null_args <- list()
    null_args$tc <- text_color(ht)[drow, dcol]
    null_args$fs <- font_size(ht)[drow, dcol]
    null_args$ft <- font(ht)[drow, dcol]
    null_args$bgc <- background_color(ht)[drow, dcol]
    # can't assign NULL to list elements
    null_args <- lapply(null_args, function (x) if (is.na(x)) NULL else x)
    nf <- number_format(ht)[[drow, dcol]] # double brackets needed here
    format_zero <- format_numbers(0, nf)
    num_fmt <- if (grepl("^0\\.0+$", format_zero)) format_zero else
          if (is.numeric(contents[drow, dcol])) 'NUMBER' else 'GENERAL'
    borders <- get_all_borders(ht, drow, dcol) # list of numerics
    border_char <- names(borders)
    border_colors <- get_all_border_colors(ht, drow, dcol)
    border_colors <- unlist(border_colors[border_char])
    border_colors[is.na(border_colors)] <- getOption('openxlsx.borderColour', 'black')
    border_styles <- get_all_border_styles(ht, drow, dcol)
    border_styles <- unlist(border_styles[border_char])
    border_styles[border_styles == "solid"] <- as.character(cut(
            unlist(borders[border_styles == "solid"]),
            c(-1, 0, 0.5, 1, 2, Inf),
            labels = c("none", "hair", "thin", "medium", "thick")
          ))
    va           <- valign(ht)[drow, dcol]

    style <- memoised_style(
            fontName       = null_args$ft,
            fontSize       = null_args$fs,
            fontColour     = null_args$tc,
            numFmt         = num_fmt,
            border         = border_char,
            borderColour   = border_colors,
            borderStyle    = border_styles,
            fgFill         = null_args$bgc, # bgFill is "for conditional formatting only"
            halign         = real_align(ht)[drow, dcol],
            valign         = switch(va, middle = 'center', va),
            textDecoration = c("bold", "italic")[c(bold(ht)[drow, dcol], italic(ht)[drow, dcol])],
            wrapText       = wrap(ht)[drow, dcol],
            textRotation   = rotation(ht)[drow, dcol]
          )
    openxlsx::addStyle(wb, sheet, style = style, rows = workbook_rows, cols = workbook_cols,
          gridExpand = TRUE)
    if (dcell$rowspan > 1 || dcell$colspan > 1) openxlsx::mergeCells(wb, sheet, cols = workbook_cols,
          rows = workbook_rows)
  }

  if (is.numeric(cw <- col_width(ht))) {
    basic_width <- 20 * ncol(ht)
    width_mult <- if (is.numeric(width(ht))) width(ht) else 0.5
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(ht)), widths = cw * width_mult * basic_width)
  }
  if (is.numeric(rh <- row_height(ht))) {
    table_height <- height(ht)
    if (is.na(table_height) || ! is.numeric(table_height)) table_height <- 1
    basic_height <- 30 * nrow(ht)
    openxlsx::setRowHeights(wb, sheet, rows = seq_len(nrow(ht)), heights = rh * basic_height * table_height)
  }

  return(wb)
}

memoised_style <- function(...) stop('This code should never be called from user code.')
