

#' @export
#'
#' @rdname to_rtf
#'
print_rtf <- function(ht, ...) cat(to_rtf(ht, ...))


#' These functions print or return an RTF table.
#'
#' @param ht A huxtable.
#' @param add_color_table Add a "colortbl" element to the RTF string. If `NULL` (the default), only
#'   add a color table if any text, background or border colors are set.
#' @param add_font_table Add a "fonttbl" element to the RTF string.  If `NULL` (the default), only
#'   add a font table if any fonts are set.
#' @param ... Arguments to pass to methods. Not currently used.
#'
#' @return `to_rtf` returns a string representing an RTF table. `print_rtf` prints the string and
#'   returns `NULL`.
#' @export
#'
#' @details
#' If color/font tables are not added, they will instead be returned as
#' `"color_table"`/`"font_table"` attributes of the string.
#'
#' @section Limitations:
#'
#' * [col_width()] can only be numeric or "pt".
#' * [rotation()] can only be 90 or 270 (i.e. text going up or down).
#'
#' @family printing functions
#'
#' @examples
#' ht <- hux(a = 1:3, b = letters[1:3])
#' to_rtf(ht)
to_rtf <- function (ht, ...) UseMethod('to_rtf')


to_rtf.huxtable <- function (ht, add_color_table = NULL, add_font_table = NULL, ...) {
  # See http://www.biblioscape.com/rtf15_spec.htm, section "Table Definitions"
  # and http://www.pindari.com/rtf3.html
  #
  # TODO: font, padding;
  # how to handle width
  # row_height and height
  # multiple col- and rowspan together may not work (perhaps need to override background colors?)
  # wrap (maybe replace spaces with nbsp?)
  if (is.null(add_color_table)) add_color_table <-
        any(! is.na(background_color(ht))) ||
        any(! is.na(text_color(ht))) ||
        any(! is.na(unlist(collapsed_border_colors(ht))))
  if (is.null(add_font_table)) add_font_table <- any(! is.na(font(ht)))
  assert_that(is.flag(add_color_table), is.flag(add_font_table))

  cc <- clean_contents(ht, type = "rtf")

  # cell borders go before \cellx, e.g.:
  # \clbrdrt\brdrs\clbrdrl\brdrs\clbrdrb\brdrs\clbrdrr\brdrs
  # per-cell, add stuff to cellx_width to create full cell spec;
  cb  <- collapsed_borders(ht)
  cbc <- collapsed_border_colors(ht)
  cbs <- collapsed_border_styles(ht)
  bgc <- background_color(ht)
  bgc[is.na(bgc)] <- 'white'

  ## MAKE COLOR TABLE -----
  ucolors <- unique(c(cbc$vert, cbc$horiz, bgc))
  ucolors_str <- ucolors
  ucolors_str[is.na(ucolors_str)] <- 'black' # we made backgrounds white before
  ucolors_str <- grDevices::col2rgb(ucolors_str)
  ucolors_str <- apply(ucolors_str, 2, function (x) {
    sprintf('\\red%d\\green%d\\blue%d', x[1], x[2], x[3])
  })
  color_tbl <- paste0(ucolors_str, ';', collapse = '')
  color_tbl <- paste0('{\\colortbl;', color_tbl, '}\n')

  ## MAKE FONT TABLE ----
  font_tbl <- ''
  if (any(! is.na(font(ht)))) {
    fonts <- unique(font(ht))
    font_tbl_body <- paste0('\\f', seq(0, along = fonts), '\\f', fonts, ';}', collapse = '\n')
    font_tbl <- paste('{\\fonttbl', font_tbl_body , '}', sep = '\n')
  }

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
  bdr_color_vert  <- sprintf('\\brdrcf%d', match(cbc$vert, ucolors))
  bdr_color_horiz <- sprintf('\\brdrcf%d', match(cbc$horiz, ucolors))

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

  bdr_def_left   <- paste0('\\clbrdrl', bdr_def_left)
  bdr_def_right  <- paste0('\\clbrdrr', bdr_def_right)
  bdr_def_top    <- paste0('\\clbrdrt', bdr_def_top)
  bdr_def_bottom <- paste0('\\clbrdrb', bdr_def_bottom)

  bdr_def_left   <- blank_where(bdr_def_left, cb$vert[, - ncol(cb$vert), drop = FALSE] == 0)
  bdr_def_right  <- blank_where(bdr_def_right, cb$vert[, -1, drop = FALSE] == 0)
  bdr_def_top    <- blank_where(bdr_def_top, cb$horiz[ - nrow(cb$horiz), , drop = FALSE] == 0)
  bdr_def_bottom <- blank_where(bdr_def_bottom, cb$horiz[ -1, , drop = FALSE] == 0)

  bdr_def <- paste0(bdr_def_top, bdr_def_left, bdr_def_bottom, bdr_def_right)

  bg_def <- sprintf('\\clcbpat%d', match(bgc, ucolors))
  bg_def <- blank_where(bg_def, is.na(bgc))

  valign_map <- c(top = '\\clvertalt', middle = '\\clvertalc', bottom = '\\clvertalb')
  valign_def <- valign_map[valign(ht)]
  # also handles rotation:
  valign_def[rotation(ht) == 90] <- '\\cltxbtlr'
  valign_def[rotation(ht) == 270] <- '\\cltxtbrl'

  text_width_twips <- 6 * 1440 # 6 inches
  col_width <- col_width(ht)
  col_width <- if (is.numeric(col_width)) {
    col_width * text_width_twips
  } else if (all(grepl('pt', col_width))) {
    as.numeric(sub('((\\d|\\.)+).*', '\\1', col_width)) * 20
  } else {
    if (! all(is.na(col_width))) warning('to_rtf can only handle numeric or "pt" col_width')
    rep(1/ncol(ht) * text_width_twips, ncol(ht))
  }
  col_width <- cumsum(col_width) # \cellx specifies the position of the RH cell edge
  cellx_def <- sprintf('\\cellx%d ', col_width)

  # cellx_def has to go along rows:
  cellx <- paste0(merge_def, bdr_def, bg_def, valign_def, rep(cellx_def, each = nrow(ht)))

  dim(cellx) <- dim(ht)

  ## MAKE CELL CONTENTS ----
  cells <- cc
  cells[bold(ht)] <- paste0('\\b ', cells[bold(ht)], '\\b0')
  cells[italic(ht)] <- paste0('\\i ', cells[italic(ht)], '\\i0')
  fs <- ceiling(font_size(ht) * 2) # 'half-points', must be integer
  cells[! is.na(fs)] <- paste0('{\\fs', fs[! is.na(fs)], ' ', cells[! is.na(fs)], '}')
  align_map <- c('left' = '\\ql', 'center' = '\\qc', 'right' = '\\qr')
  cells <- paste0(align_map[real_align(ht)], cells)
  cells <- paste0('\\pard\\intbl', cells, '\\cell')
  dim(cells) <- dim(ht)

  ## CREATE ROWS ----
  cellx_rows <- apply(cellx, 1, paste0, collapse = '\n')
  cell_content_rows <- apply(cells, 1, paste0, collapse = '\n')
  # the braces keep the colortbl and fonttbl local
  rows <- paste0('{\n\\trowd\n', cellx_rows, cell_content_rows, '\n\\row\n}\n')

  ## EVERYTHING TOGETHER ----
  result <- paste(rows, collapse = '\n')
  if (add_color_table) result <- paste0(color_tbl, result) else attr(result, 'color_table') <- color_tbl
  if (add_font_table)  result <- paste0(font_tbl, result)  else attr(result, 'font_table') <- font_tbl

  return(result)
  # each row starts \trowd and ends \row
  # column widths are in twips, 1 twip = 1/20pt = 1/1440 inch
  # defined after \trowd like:
  # \cellxWIDTH
  # \cellxWIDTH...
  # cell borders go before \cellx, e.g.:
  #
  # \clbrdrt\brdrs\clbrdrl\brdrs\clbrdrb\brdrs\clbrdrr\brdrs
  #  \cellx1000
  #
  #  That's top,left,bottom,right.
  #
  #  Borders are \brdrs (single), \brdrth (double thickness), \brdrdb (double), \brdrdot, \brdrdash,
  #  \brdrhair (very thin?) or \brdrwN where N is border width in twips!!!
  #  \brdrcfN is color of border (index into \colortbl...!)
  #
  #  \brspN is cell padding in twips...
  #


  #
  # font needs a "font table", should one provide one's own

}
