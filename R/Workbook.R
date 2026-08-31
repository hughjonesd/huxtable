#' @import assertthat
NULL


#' Convert a huxtable for Excel
#'
#' If the `openxlsx` package is installed, Huxtables can be converted to
#' [openxlsx::openxlsx()] Worbook objects, for use in Excel documents.
#'
#' @param ht A huxtable.
#' @param Workbook An existing `Workbook` object. By default, a new workbook will be created.
#' @param sheet Name for the worksheet where the huxtable will be created. The
#'   worksheet will be created if it doesn't exist already.
#' @param write_caption If `TRUE`, print any caption in the row above or below the table.
#' @param start_row,start_col Number. Write data starting at the given row and column.
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
#' * Caption width.
#'
#' Huxtable tries to guess appropriate widths and height for rows and columns; numeric [width()] and
#' [height()] are treated as scaling factors.
#'
#' Contents are only stored as numbers if a whole column is "numeric", i.e. can
#' be converted by [as.numeric()]). Otherwise, they are stored as text.
#'
#' @return An object of class `Workbook`.
#' @export
#'
#' @examples
#' wb <- as_Workbook(jams)
#'
#' \dontrun{
#' openxlsx::saveWorkbook(
#'   wb,
#'   "my-excel-file.xlsx"
#' )
#' }
#'
#' # multiple sheets in a single workbook:
#' wb <- openxlsx::createWorkbook()
#' wb <- as_Workbook(jams,
#'   Workbook = wb, sheet = "sheet1"
#' )
#' wb <- as_Workbook(
#'   hux("Another", "huxtable"),
#'   Workbook = wb,
#'   sheet = "sheet2"
#' )
as_Workbook <- function(ht, ...) UseMethod("as_Workbook")


memo_env <- new.env()

#' @export
#' @rdname as_Workbook
as_Workbook.huxtable <- function(ht,
                                 Workbook = NULL,
                                 sheet = "Sheet 1",
                                 write_caption = TRUE,
                                 start_row = 1,
                                 start_col = 1,
                                 ...) {
  assert_package("as_Workbook", "openxlsx")
  assert_that(is.string(sheet), is.count(start_row), is.count(start_col))

  if (!exists("memoised_createStyle", where = memo_env)) {
    memo_env$memoised_createStyle <- memoise::memoise(openxlsx::createStyle)
  }
  wb <- if (missing(Workbook) || is.null(Workbook)) openxlsx::createWorkbook() else Workbook
  if (!sheet %in% names(wb)) openxlsx::addWorksheet(wb, sheet)
  notes <- clean_table_notes(ht)
  top_cap <- write_excel_caption(
    wb, ht, sheet, write_caption, start_row, start_col,
    bottom_offset = length(notes)
  )

  contents <- clean_contents(ht, output_type = "excel") # character matrix

  write_excel_contents(wb, sheet, contents, start_row, start_col, top_cap)

  apply_excel_styles(wb, sheet, ht, contents, start_row, start_col, top_cap)

  if (length(notes) > 0L) {
    note_rows <- start_row + as.integer(top_cap) + nrow(ht) + seq_along(notes) - 1L
    note_cols <- start_col - 1L + seq_len(ncol(ht))
    note_style <- openxlsx::createStyle(halign = "left", wrapText = TRUE)
    for (i in seq_along(notes)) {
      openxlsx::writeData(
        wb, sheet, notes[[i]],
        startRow = note_rows[[i]], startCol = start_col
      )
      if (ncol(ht) > 1L) {
        openxlsx::mergeCells(wb, sheet, cols = note_cols, rows = note_rows[[i]])
      }
    }
    openxlsx::addStyle(
      wb, sheet, style = note_style, rows = note_rows, cols = note_cols,
      gridExpand = TRUE
    )
  }

  set_excel_dimensions(wb, sheet, ht, start_row, start_col)

  return(wb)
}

#' Write caption to an Excel worksheet
#'
#' @noRd
write_excel_caption <- function(wb, ht, sheet, write_caption, start_row, start_col,
                                bottom_offset = 0L) {
  cap <- caption(ht)
  top_cap <- write_caption && !is.na(cap) && get_caption_vpos(ht) == "top"
  cap_row <- if (top_cap) start_row else start_row + nrow(ht) + bottom_offset
  if (write_caption && !is.na(cap)) {
    openxlsx::writeData(wb, sheet, x = cap, startRow = cap_row)
    cap_style <- openxlsx::createStyle(halign = get_caption_hpos(ht))
    openxlsx::addStyle(wb, sheet,
      style = cap_style, rows = cap_row, cols = seq_len(ncol(ht)),
      gridExpand = TRUE
    )
    openxlsx::mergeCells(wb, sheet, cols = seq_len(ncol(ht)), rows = cap_row)
  }
  top_cap
}

#' Write huxtable contents to an Excel worksheet
#'
#' @noRd
write_excel_contents <- function(wb, sheet, contents, start_row, start_col, top_cap) {
  contents <- as.data.frame(contents, stringsAsFactors = FALSE)
  nr <- nrow(contents)
  if (nr == 0 || ncol(contents) == 0) return(invisible(NULL))

  is_numeric <- suppressWarnings(!is.na(as.numeric(as.matrix(contents))))
  dim(is_numeric) <- dim(contents)
  same_as_previous <- apply(is_numeric[-1, , drop = FALSE] ==
    is_numeric[-nr, , drop = FALSE], 1, all)
  starts <- c(1, which(!same_as_previous) + 1)
  ends <- c(starts[-1] - 1, nr)

  for (i in seq_along(starts)) {
    rows <- starts[i]:ends[i]
    insert <- contents[rows, , drop = FALSE]
    numeric_cols <- is_numeric[starts[i], ]
    insert[numeric_cols] <- lapply(insert[numeric_cols], as.numeric)
    openxlsx::writeData(wb, sheet, insert,
      startRow = start_row + top_cap + starts[i] - 1, startCol = start_col,
      colNames = FALSE, rowNames = FALSE, borders = "none", borderStyle = "none"
    )
  }
}

#' Apply styles to an Excel worksheet
#'
#' @noRd
apply_excel_styles <- function(wb, sheet, ht, contents, start_row, start_col, top_cap) {
  dcells <- display_cells(ht, all = FALSE)
  style_groups <- list()
  for (r in seq_len(nrow(dcells))) {
    dcell <- dcells[r, ]
    drow <- dcell$display_row
    dcol <- dcell$display_col

    workbook_rows <- start_row - 1 + seq(drow, dcell$end_row)
    if (top_cap) workbook_rows <- workbook_rows + 1
    workbook_cols <- start_col - 1 + seq(dcol, dcell$end_col)

    null_args <- list()
    null_args$tc <- text_color(ht)[drow, dcol]
    null_args$fs <- font_size(ht)[drow, dcol]
    null_args$ft <- font(ht)[drow, dcol]
    null_args$bgc <- background_color_with_fallback(ht)[drow, dcol]
    null_args <- lapply(null_args, function(x) if (is.na(x)) NULL else x)

    nf <- number_format(ht)[[drow, dcol]]
    format_zero <- format_numbers(0, nf)
    num_fmt <- if (grepl("^0\\.0+$", format_zero)) format_zero else if (is.numeric(contents[drow, dcol])) "NUMBER" else "GENERAL"

    borders <- get_all_borders(ht, drow, dcol)
    border_char <- names(borders)
    border_colors <- get_all_border_colors(ht, drow, dcol)
    border_colors <- unlist(border_colors[border_char])
    border_colors[is.na(border_colors)] <- getOption("openxlsx.borderColour", "black")

    border_styles <- get_all_border_styles(ht, drow, dcol)
    border_styles <- unlist(border_styles[border_char])
    border_styles[border_styles == "solid"] <- as.character(cut(
      unlist(borders[border_styles == "solid"]),
      c(-1, 0, 0.5, 1, 2, Inf),
      labels = c("none", "hair", "thin", "medium", "thick")
    ))

    va <- valign(ht)[drow, dcol]

    style <- memo_env$memoised_createStyle(
      fontName = null_args$ft,
      fontSize = null_args$fs,
      fontColour = null_args$tc,
      numFmt = num_fmt,
      border = border_char,
      borderColour = border_colors,
      borderStyle = border_styles,
      fgFill = null_args$bgc, # bgFill is "for conditional formatting only"
      halign = real_align(ht)[drow, dcol],
      valign = switch(va,
        middle = "center",
        va
      ),
      textDecoration = c("bold", "italic")[c(bold(ht)[drow, dcol], italic(ht)[drow, dcol])],
      wrapText = wrap(ht)[drow, dcol],
      textRotation = rotation(ht)[drow, dcol]
    )
    style_rows <- rep(workbook_rows, each = length(workbook_cols))
    style_cols <- rep(workbook_cols, times = length(workbook_rows))
    style_group <- which(vapply(style_groups, function(x) {
      identical(x$style, style)
    }, logical(1)))[1]
    if (is.na(style_group)) {
      style_groups[[length(style_groups) + 1]] <- list(
        style = style, rows = style_rows, cols = style_cols
      )
    } else {
      style_groups[[style_group]]$rows <- c(style_groups[[style_group]]$rows, style_rows)
      style_groups[[style_group]]$cols <- c(style_groups[[style_group]]$cols, style_cols)
    }
    if (dcell$rowspan > 1 || dcell$colspan > 1) {
      openxlsx::mergeCells(wb, sheet,
        cols = workbook_cols,
        rows = workbook_rows
      )
    }
  }

  for (group in style_groups) {
    openxlsx::addStyle(wb, sheet,
      style = group$style, rows = group$rows, cols = group$cols
    )
  }
}

#' Set dimensions for an Excel worksheet
#'
#' @noRd
set_excel_dimensions <- function(wb, sheet, ht, start_row, start_col) {
  cw <- col_width(ht)
  if (!is.numeric(cw) || anyNA(cw)) cw <- rep(1 / ncol(ht), ncol(ht))
  basic_width <- 20 * ncol(ht)
  w <- width(ht)
  if (!is.numeric(w) || is.na(w)) w <- 0.5
  openxlsx::setColWidths(wb, sheet,
    cols = start_col - 1 + seq_len(ncol(ht)),
    widths = cw * w * basic_width
  )

  if (is.numeric(rh <- row_height(ht)) && length(rh) > 0) {
    table_height <- height(ht)
    if (is.na(table_height) || !is.numeric(table_height)) table_height <- 1
    basic_height <- 30 * nrow(ht)
    openxlsx::setRowHeights(wb, sheet,
      rows = start_row - 1 + seq_len(nrow(ht)),
      heights = rh * basic_height * table_height
    )
  }
}
