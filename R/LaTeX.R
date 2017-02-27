

#' Create LaTeX Representing a Huxtable
#'
#' @param ht A huxtable.
#' @param ...
#'
#' @return \code{to_latex} returns a string. \code{print_latex} prints the string and returns \code{NULL}.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' print_latex(ht)
to_latex <- function (ht, ...) UseMethod('to_latex')

#' @export
to_latex.huxtable <- function (ht, ...){
  res <- build_tabular(ht)
  # convenience function
  # don't indent or pandoc may treat it as verbatim
  res <- paste0('
\\let\\Oldarrayrulewidth\\relax
\\newlength\\Oldarrayrulewidth
\\providecommand{\\Cline}[2]{}
\\renewcommand{\\Cline}[2]{%
\\noalign{\\global\\setlength{\\Oldarrayrulewidth}{\\arrayrulewidth}}%
\\noalign{\\global\\setlength{\\arrayrulewidth}{#1}}\\cline{#2}%
\\noalign{\\global\\setlength{\\arrayrulewidth}{\\Oldarrayrulewidth}}}', res)

  if (! is.na(cap <- caption(ht))) {
    cap <- paste0('\\caption{', cap, '}\n')
    res <- if (caption_pos(ht) == 'top') paste0(cap, res) else paste0(res, cap)
  }

  # table position
  pos_text <- switch(position(ht),
    left   = c('\\begin{raggedright}', '\\par\\end{raggedright}'),
    center = c('\\begin{centering}',   '\\par\\end{centering}'),
    right  = c('\\begin{raggedleft}',  '\\par\\end{raggedleft}')
  )
  res <- paste0(pos_text[1], res, pos_text[2], '\n')

  res <- paste0('\\begin{table}[h]\n', res, '\\end{table}\n')

  return(res)
}

build_tabular <- function(ht) {
  res <- '\\begin{tabularx}'
  tw <- width(ht)
  if (is.numeric(tw)) tw <- paste0(tw, '\\textwidth')
  res <- paste0(res, '{', tw,'}')

  col_width <- col_width(ht)
  if (all(is.na(col_width))) col_width <- rep(1/ncol(ht), ncol(ht))
  if (is.numeric(col_width)) {
    col_width <- paste0(col_width, '\\textwidth')
  }
  colspec <- '{'
  for (mycol in 1:ncol(ht)) {
    col_align <- align(ht)[,mycol]
    if (length(unique(col_align)) > 1) warning('Cannot use multiple alignments in a single column in LaTeX, at column ',
      mycol)
    col_align <- col_align[1]

    # colspec for tabularx is >{code}char{width}
    # code is code to put first e.g. \raggedright
    # char can be p, m or b
    # width is e.g. 0.3\textwidth, but can also have various representations
    # a good way to do % of table width is tabularx X type; then
    # you can do
    # >{\hsize=1.2\hsize}X; \hsize is 1 for evenly sized columns
    # but, can't do vertical align easily using tabularx. Can do it with 'renewcommand'.

    colspec_align_str <- switch(as.character(col_align),
      left    = '>{\\raggedright}',
      right   = '>{\\raggedleft}',
      decimal = '>',
      centre  = ,
      '>{\\centering}')

    col_valign <- valign(ht)[,mycol]
    if (length(unique(col_valign)) > 1) warning(
      'Cannot use multiple vertical alignments in a single column in LaTeX, at column ', mycol)
    col_valign <- col_valign[1]

    colspec_valign_str <- switch(as.character(col_valign), middle = 'm', bottom = 'b', top = , 'p')
    if (isTRUE(col_align == 'decimal')) colspec_valign_str <- 'S'
    colspec_valign_str <- paste0(colspec_valign_str, '{', col_width[mycol],  '}')
    colspec <- paste0(colspec, colspec_align_str, colspec_valign_str, ' ')
  }
  colspec <- paste0(colspec, '}')
  res <- paste0(res, colspec, '\n')

  display_cells <- display_cells(ht)
  res <- paste0(res, build_clines_for_row(ht, row = 0))

  for (myrow in 1:nrow(ht)) {
    row_contents <- character(0)
    added_right_border <- FALSE
    for (mycol in 1:ncol(ht)) {
      dcell <- display_cells[display_cells$row == myrow & display_cells$col == mycol,]
      drow <- dcell$display_row
      dcol <- dcell$display_col
      # we print out contents for (a) unshadowed cells which are not multirow
      # (b) shadowed cells which are in the last row (but first column) of a multirow
      contents <- ''
      rs <- rowspan(ht)[drow, dcol]
      if (! dcell$shadowed && rs == 1) contents <- build_cell_contents(ht, myrow, mycol)
      if (dcell$shadowed && drow + rs - 1  == myrow && dcol == mycol) contents <- build_cell_contents(ht, myrow, mycol)

      # print out cell_color and borders from display cell rather than actual cell
      # but only for left hand cells (which will be multicolumn{colspan} )
      if (! is.na(cell_color <- background_color(ht)[drow, dcol]) && mycol == dcol) {
        cell_color <- latex_color(cell_color)
        cell_color <- paste0('\\cellcolor[RGB]{', cell_color, '}')
        contents <- paste0(cell_color, ' ', contents)
      }

      # multirows are moved to last line...
      if (dcell$shadowed && drow + rs - 1  == myrow && dcol == mycol) {
        # the ctb switch may only work with v recent multirow
        # ctb <- switch(valign(ht)[myrow, mycol], top = 't', bottom = 'b', middle = 'c')
        # * is 'standard width', could be more specific
        contents <- paste0('\\multirow{-', rs,'}{*}{', contents,'}')
      }

      # must be this way round: multirow inside multicolumn
      # (a) if not a shadowed cell, put multicol round content;
      # (b) if shadowed from directly above (ie. a left cell of a multirow/multicol group), use an empty multicol
      # and add appropriate borders
      # (c) otherwise do not print anything
      if (! dcell$shadowed || mycol == dcol) {
        cs <- colspan(ht)[drow, dcol]
        lcr <- switch(align(ht)[drow, dcol], left   = 'l', right  = 'r', center = 'c')
        # only add left borders if we haven't already added a right border!
        lb <- if (left_border(ht)[drow, dcol] > 0 && ! added_right_border) '|' else ''
        if (right_border(ht)[drow, dcol] > 0) {
          rb <- '|'
          added_right_border <- TRUE
        } else {
          rb <- ''
          added_right_border <- FALSE
        }
        contents <- paste0('\\multicolumn{', cs,'}{', lb, lcr, rb ,'}{', contents,'}')
      }

      row_contents[mycol] <- contents

    } # next cell
    row_contents <- row_contents[nzchar(row_contents)] # if we've printed nothing, don't print an & for it
    row_contents <- paste(row_contents, collapse = ' & ')
    res <- paste0(res, row_contents, ' \\tabularnewline\n')

    # add top/bottom borders
    res <- paste0(res, build_clines_for_row(ht, myrow))
  } # next row

  res <- paste0(res, '\\end{tabularx}\n')

  return(res)
}

build_cell_contents <- function(ht, row, col) {
  contents <- clean_contents(ht, row, col, type = 'latex')
  if (! is.na(text_color <- text_color(ht)[row, col])) {
    text_color <- latex_color(text_color)
    contents <- paste0('\\textcolor[RGB]{', text_color, '}{', contents,'}')
  }
  if (bold(ht)[row, col]) {
    contents <- paste0('\\textbf{', contents, '}')
  }
  if (italic(ht)[row, col]) {
    contents <- paste0('\\textit{', contents, '}')
  }

  return(contents)
}

build_clines_for_row <- function(ht, row) {
  # row can be from "0" for the top; up to nrow
  # add top/bottom borders
  # where a cell is shadowed, we don't want to add a top border (it'll go thru the middle)
  # bottom borders of a shadowed cell are fine, but come from the display cell.
  display_cells <- display_cells(ht)
  dcells_this_row <- unique(display_cells[display_cells$row == row, c('display_row', 'display_col')])
  this_bottom <- rep(0, ncol(ht))

  blank_line_color <- rep(latex_color('white'), ncol(ht)) # white by default, I guess...
  for (i in seq_len(nrow(dcells_this_row))) {
    drow <- dcells_this_row[i, 'display_row']
    dcol <- dcells_this_row[i, 'display_col']
    dcell_bottom <- drow + rowspan(ht)[drow, dcol] - 1
    dcell_right <- dcol + colspan(ht)[drow, dcol] - 1
    # Print bottom border if we are at bottom of the display cell
    if (row == dcell_bottom) {
      bb <- bottom_border(ht)[drow, dcol]
      this_bottom[dcol:dcell_right] <- bb
    }
    # Use color if we are in middle of display cell
    if (row < dcell_bottom & ! is.na(color <- background_color(ht)[drow, dcol])) {
      blank_line_color[dcol:dcell_right] <- latex_color(color)
    }
  }
  blanks <- paste0('>{\\arrayrulecolor[RGB]{', blank_line_color ,'}}-')

  dcells_next_row <- unique(display_cells[display_cells$row == row + 1, c('display_row', 'display_col')])
  next_top <- rep(0, ncol(ht))
  for (i in seq_len(nrow(dcells_next_row))) {
    drow <- dcells_next_row[i, 'display_row']
    dcol <- dcells_next_row[i, 'display_col']
    # are we at the top of this dcell? If not...
    if (row + 1 != drow) next
    tb <- top_border(ht)[drow, dcol]
    cs <- colspan(ht)[drow, dcol]
    next_top[dcol:(dcol - 1 + cs)] <- tb
  }
  borders <- pmax(this_bottom, next_top) # the 'collapse' model

  cells <- which(borders > 0)
  if (any(borders > 0)) {
    border_lines <- rep('>{\\arrayrulecolor{black}}-', ncol(ht))
    hhlinechars <- ifelse(borders > 0, border_lines , blanks)
    # add in |.
    # For each hhlinechar (1 real column), look at above (and/or below?) left/right borders
    # | placed at left of corresponding - for left border;
    # at right of - for right border
    vertlines <- rep('', ncol(ht) + 1)
    borders <- compute_vertical_borders(ht, row)
    vertlines[borders > 0] <- '>{\\arrayrulecolor{black}}|'

    hhlinechars <- paste0(hhlinechars, vertlines[-1], collapse = '')
    hhlinechars <- paste0(vertlines[1], hhlinechars)
    hhline <- paste0('\\hhline{', hhlinechars ,'}\n')
    hhline <- paste0(hhline, '\\arrayrulecolor{black}\n') # don't let arrayrulecolor spill over
    return(hhline)
    # clines don't play nicely with cell color
    # borders <- paste0(borders, 'pt')
    # return(paste0('\\Cline{', borders[cells],'}{', cells, '-', cells, '}\n', collapse=' '))
  } else {
    return('')
  }
}

compute_vertical_borders <- function (ht, row) {
  display_cells <- display_cells(ht)
  dcells_this_row <- unique(display_cells[display_cells$row == row, c('display_row', 'display_col')])

  lbs <- rep(0, ncol(ht) + 1)
  rbs <- rep(0, ncol(ht) + 1)
  for (i in seq_len(nrow(dcells_this_row))) {
    drow <- dcells_this_row[i, 'display_row']
    dcol <- dcells_this_row[i, 'display_col']
    right_col <- dcol + colspan(ht)[drow, dcol] # don't deduct 1 because first rbs is always 0
    lbs[dcol] <- left_border(ht)[drow, dcol]
    rbs[right_col] <- right_border(ht)[drow, dcol]
  }

  pmax(lbs, rbs)
}

latex_color <- function (r_color) paste0(as.vector(col2rgb(r_color)), collapse = ', ')

#' @export
#'
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}
