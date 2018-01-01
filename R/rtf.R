

#' @export
#' @rdname to_rtf
print_rtf <- function(ht, fc_tables = rtf_fc_tables(ht), ...) {
  cat(to_rtf(ht, fc_tables, ...))
}


#' Create RTF representing a huxtable
#'
#' These functions print or return an RTF character string.
#'
#' @param ht A huxtable.
#' @param fc_tables See [rtf_fc_tables()].
#' @param ... Arguments to pass to methods.
#'
#' @return `to_rtf` returns a string representing an RTF table. The `fc_tables` attribute of the
#'   returned string will contain the `fc_tables` object that was passed in (or autocreated).
#'   `print_rtf` prints the string and returns `NULL`.
#' @export
#'
#' @details
#' RTF files use a single per-document table for colors, and one for fonts. If you are printing
#' multiple huxtables in a document, you need to make sure that the font and color table is
#' set up correctly and that the RTF tables refer back to them. See [rtf_fc_tables()].
#'
#' 1. Prepare all the huxtables;
#' 2. Call [rtf_fc_tables()], passing in all the huxtables;
#' 3. Print the `rtfFCTables` object in the RTF document header;
#' 4. Pass in the `rtfFCTables` object to each call to `print_rtf`.
#'
#' @section Limitations:
#'
#' * rmarkdown's `rtf_document` can't yet print out customized color tables, so custom fonts
#'   and colors won't work in this context.
#' * [col_width()] and [width()] can only be numeric or "pt".
#' * [wrap()] has no effect: cell contents always wrap.
#' * [rotation()] can only be 90 or 270, i.e. text going up or down.
#'
#' @family printing functions
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_rtf(ht)
to_rtf <- function (ht, ...) UseMethod('to_rtf')


#' @export
#' @rdname to_rtf
to_rtf.huxtable <- function (ht, fc_tables = rtf_fc_tables(ht), ...) {
  # See http://www.biblioscape.com/rtf15_spec.htm, section "Table Definitions"
  # and http://www.pindari.com/rtf3.html
  # the O'Reilly guide is also very helpful

  assert_that(inherits(fc_tables, 'rtfFCTables'))
  color_index <- function (color) {
    res <- match(color, fc_tables$colors)
    if (any(is.na(res) & ! is.na(color))) warning('Color not found in color table.\n',
          '(Did you change colors after calling `rtf_fc_tables`?)')
    res
  }

  cb  <- collapsed_borders(ht)
  cbc <- collapsed_border_colors(ht)
  cbs <- collapsed_border_styles(ht)
  bgc <- background_color(ht)
  tc <- text_color(ht)

  ## MAKE CELLX DEFINITIONS ----

  left_merge <- ifelse(colspan(ht) > 1, '\\clmgf', '')
  top_merge  <- ifelse(rowspan(ht) > 1, '\\clvmgf', '')
  dc <- display_cells(ht, all = TRUE)
  right_merge <- ifelse(dc$col > dc$display_col, '\\clmrg', '')
  bottom_merge <- ifelse(dc$row > dc$display_row, '\\clvmrg', '')
  merge_def <- paste0(left_merge, top_merge, right_merge, bottom_merge)

  bdr_width_vert  <- paste0('\\brdrw', cb$vert * 20)
  bdr_width_horiz <- paste0('\\brdrw', cb$horiz * 20)
  bdr_style_map <- c(
          'solid'  = '\\brdrs',
          'double' = '\\brdrdb',
          'dashed' = '\\brdrdash',
          'dotted' = '\\brdrdot'
        )
  bdr_style_vert  <- bdr_style_map[cbs$vert]
  bdr_style_horiz <- bdr_style_map[cbs$horiz]
  bdr_color_vert  <- sprintf('\\brdrcf%d', color_index(cbc$vert))
  bdr_color_horiz <- sprintf('\\brdrcf%d', color_index(cbc$horiz))
  bdr_color_vert <- blank_where(bdr_color_vert, is.na(cbc$vert))
  bdr_color_horiz <- blank_where(bdr_color_horiz, is.na(cbc$horiz))

  # these are matrices (horiz = nr+1 * nc, vert = nr * nc+1).
  # For cell (i, j), top and left are i, j; right is i, j+1; bottom is i+1,j; in respective matrices
  bdr_def_vert <- paste0(bdr_style_vert, bdr_width_vert, bdr_color_vert)
  bdr_def_horiz <- paste0(bdr_style_horiz, bdr_width_horiz, bdr_color_horiz)
  dim(bdr_def_vert) <- dim(cb$vert)
  dim(bdr_def_horiz) <- dim(cb$horiz)

  bdr_def_left   <- bdr_def_vert[, - ncol(bdr_def_vert), drop = FALSE]
  bdr_def_right  <- bdr_def_vert[, -1, drop = FALSE]
  bdr_def_top    <- bdr_def_horiz[ - nrow(bdr_def_horiz), , drop = FALSE]
  bdr_def_bottom <- bdr_def_horiz[ -1, , drop = FALSE]

  bdr_def_left   <- blank_where(bdr_def_left, cb$vert[, - ncol(cb$vert), drop = FALSE] == 0)
  bdr_def_right  <- blank_where(bdr_def_right, cb$vert[, -1, drop = FALSE] == 0)
  bdr_def_top    <- blank_where(bdr_def_top, cb$horiz[ - nrow(cb$horiz), , drop = FALSE] == 0)
  bdr_def_bottom <- blank_where(bdr_def_bottom, cb$horiz[ -1, , drop = FALSE] == 0)

  bdr_def_left   <- paste0('\\clbrdrl', bdr_def_left)
  bdr_def_right  <- paste0('\\clbrdrr', bdr_def_right)
  bdr_def_top    <- paste0('\\clbrdrt', bdr_def_top)
  bdr_def_bottom <- paste0('\\clbrdrb', bdr_def_bottom)

  bdr_def <- paste0(bdr_def_top, bdr_def_left, bdr_def_bottom, bdr_def_right)

  bg_def <- sprintf('\\clcbpat%d', color_index(bgc))
  bg_def <- blank_where(bg_def, is.na(bgc))

  valign_map <- c(top = '\\clvertalt', middle = '\\clvertalc', bottom = '\\clvertalb')
  valign_def <- valign_map[valign(ht)]
  # also handles rotation:
  valign_def[rotation(ht) == 90] <- '\\cltxbtlr'
  valign_def[rotation(ht) == 270] <- '\\cltxtbrl'

  wrap_def <- ifelse(wrap(ht), '', '\\clNoWrap')
  pad_def <- sprintf('\\clpadfl3\\clpadl%d \\clpadft3\\clpadt%d \\clpadfb3\\clpadb%d \\clpadfr3\\clpadr%d ',
        left_padding(ht)   * 20,
        top_padding(ht)    * 20,
        bottom_padding(ht) * 20,
        right_padding(ht)  * 20)

  table_width <- width(ht)
  col_width <- col_width(ht)

  col_width <- if (is.numeric(col_width)) {
    col_width
  } else if (all(grepl('pt', col_width))) {
    as.numeric(sub('((\\d|\\.)+).*', '\\1', col_width)) * 20
  } else {
    if (! all(is.na(col_width))) warning('to_rtf can only handle numeric or "pt" col_width')
    rep(1/ncol(ht), ncol(ht))
  }

  if (! is.numeric(table_width)) {
    warning('to_rtf can only handle numeric table width')
    table_width <- get_default_properties('width')[[1]]
  }
  text_width_twips <- 6 * 72 * 20 # assumed 6 inches wide, 1 inch = 72 pt, 1 pt = 20 twips
  col_width <- col_width * text_width_twips * table_width
  # \cellx specifies the position of the RH cell edge:
  right_edges <- ceiling(cumsum(col_width))

  cellx_def <- sprintf('\\cellx%d ', right_edges)

  # cellx_def has to go along rows:
  cellx <- paste0(merge_def, bdr_def, bg_def, valign_def, wrap_def, pad_def,
        rep(cellx_def, each = nrow(ht)))

  dim(cellx) <- dim(ht)

  ## MAKE CELL CONTENTS ----
  cc <- clean_contents(ht, type = 'rtf')
  cells <- paste0('{', cc, '}')
  cells[bold(ht)] <- paste0('\\b ', cells[bold(ht)], '\\b0')
  cells[italic(ht)] <- paste0('\\i ', cells[italic(ht)], '\\i0')
  fs <- ceiling(font_size(ht) * 2) # 'half-points', must be integer
  cells[! is.na(fs)] <- paste0('{\\fs', fs[! is.na(fs)], ' ', cells[! is.na(fs)], '}')
  cells[! is.na(tc)] <- paste0('{\\cf', match(tc[! is.na(tc)], fc_tables$colors), ' ',
        cells[! is.na(tc)], '}')

  ft <- font(ht)
  findex <-  match(ft[! is.na(ft)], fc_tables$fonts) - 1
  if (any(is.na(findex))) warning('Font not found in font table.\n',
        '(Did you change a font after calling `rtf_fc_table`?)')
  cells[! is.na(ft)] <- paste0('{\\f', findex, ' ', cells[! is.na(ft)], '}')

  align_map <- c('left' = '\\ql', 'center' = '\\qc', 'right' = '\\qr')
  cells <- paste0(align_map[real_align(ht)], cells)
  cells <- paste0('\\pard\\intbl', cells, '\\cell')
  dim(cells) <- dim(ht)

  ## CREATE ROWS ----
  cellx_rows <- apply(cellx, 1, paste0, collapse = '\n')
  cell_content_rows <- apply(cells, 1, paste0, collapse = '\n')

  row_align_map <- c('left' = '\\trql ', 'center' = '\\trqc ', 'right' = '\\trqr ')
  row_align <- row_align_map[position(ht)]

  rh <- row_height(ht)
  table_height <- height(ht)
  row_heights <- ''
  if (any(! is.na(rh)) || ! is.na(table_height)) {
    if (! is.numeric(rh) && ! all(is.na(rh))) warning('to_rtf can only handle numeric row_height.')
    if (! is.numeric(table_height) && ! is.na(table_height)) warning(
          'to_rtf can only handle numeric table height.')
    if (! is.numeric(table_height) || is.na(table_height)) table_height <- 0.33
    page_height <- 10 * 72 * 20 # 10 inches in twips
    if (any(is.na(as.numeric(rh)))) rh <- rep(1/nrow(ht), nrow(ht))
    rh <- ceiling(rh * page_height * table_height)
    row_heights <- sprintf('\\trrh%d ', rh)
  }
  rows <- paste0('{\n\\trowd\n', row_align, row_heights, cellx_rows, cell_content_rows, '\n\\row\n}\n')

  ## CAPTION ----

  caption <- caption(ht)
  cap_align <- align_map[get_caption_hpos(ht)]
  caption_par <- if (is.na(caption)) '' else sprintf('{\\pard %s {%s} \\par}', cap_align, caption)


  ## PASTE EVERYTHING TOGETHER ----
  result <- paste(rows, collapse = '\n')
  result <- if (grepl('top', caption_pos(ht))) paste(caption_par, result, sep = '\n') else paste(
        result, caption_par, sep = '\n')
  attr(result, 'fc_tables') <- fc_tables

  return(result)
}


#' Create RTF font and color tables
#'
#' @param ... One or more objects of class `huxtable`.
#' @param extra_fonts Extra fonts to include. These will be first in the fonts table.
#' @param extra_colors Extra colors to include, as R color names.
#'
#' @return An object of class `rtfFCTables`. This is a list containing two items: `"fonts"`
#' is a character vector of unique font names; `"colors"` is a character vector of unique color
#' names.
#'
#' @details
#' RTF documents have a single table of fonts, and a table of colors, in the RTF header. To
#' create font and color tables for multiple huxtables, use this command. You can `print` the
#' returned object in the RTF header. Pass it to [print_rtf()] or [to_rtf()] to ensure that
#' huxtables print out the correct colour references.
#' @export
#'
#' @examples
#'
#' # Printing multiple huxtables
#' ht <- huxtable("Blue with red border")
#' ht <- set_all_borders(ht, 1)
#' ht <- set_all_border_colors(ht, "red")
#' background_color(ht) <- "blue"
#'
#' ht2 <- huxtable("Dark green text")
#' text_color(ht2) <- "darkgreen"
#' fc_tbls <- rtf_fc_tables(ht, ht2)
#'
#' # In the document header:
#' print(fc_tbls)
#'
#' # In the document body:
#' print_rtf(ht, fc_tables = fc_tbls)
#' print_rtf(ht2, fc_tables = fc_tbls)
rtf_fc_tables <- function (..., extra_fonts = 'Times', extra_colors = character(0)) {
  hts <- list(...)
  assert_that(all(sapply(hts, is_huxtable)))

  fonts <- unlist(lapply(hts, function (ht) font(ht)))
  fonts <- unique(c(extra_fonts, fonts))
  fonts <- stats::na.omit(fonts)

  colors <- unlist(lapply(hts, function (ht) {
    c(text_color(ht), background_color(ht), unlist(collapsed_border_colors(ht)))
  }))
  colors <- unique(c(extra_colors, colors))
  colors <- stats::na.omit(colors)

  result <- list()
  result$fonts <- fonts
  result$colors <- colors
  class(result) <- 'rtfFCTables'

  result
}


font_table_string <- function (x) {
  font_tbl_body <- ''
  font_tbl_body <- paste0('  {\\f', seq(0, along = x$fonts), ' ', x$fonts, ';}', collapse = '\n')
  paste('{\\fonttbl', font_tbl_body , '}', sep = '\n')
}


color_table_string <- function (x) {
  colors_str <- grDevices::col2rgb(x$colors)
  colors_str <- apply(colors_str, 2, function (clr) {
    sprintf('\\red%d\\green%d\\blue%d', clr[1], clr[2], clr[3])
  })
  color_tbl <- paste0(colors_str, ';', collapse = '')
  color_tbl <- paste0('{\\colortbl;', color_tbl, '}\n')

  return(color_tbl)
}


format.rtfFCTables <- function (x, ...) {
  paste(font_table_string(x), color_table_string(x), sep = '\n')
}


print.rtfFCTables <- function (x, ...) {
  cat(format(x, ...))
}
