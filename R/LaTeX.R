

#' @export
#'
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}

#' Create LaTeX Representing a Huxtable
#'
#' @param ht A huxtable.
#' @param tabular_only Return only the LaTeX tabular, not the surrounding float.
#' @param ... Arguments to pass to methods.
#'
#' @return \code{to_latex} returns a string. \code{print_latex} prints the string and returns \code{NULL}.
#' @export
#'
#' @family printing functions
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' print_latex(ht)
to_latex <- function (ht, ...) UseMethod('to_latex')

#' @export
#' @rdname to_latex
to_latex.huxtable <- function (ht, tabular_only = FALSE, ...){
  res <- build_tabular(ht)
  if (tabular_only) return(res)


  if (! is.na(height <- height(ht))) {
    if (is.numeric(height)) height <- paste0(height, '\\textheight')
    res <- paste0('\\resizebox*{!}{', height, '}{\n', res, '\n}')
  }

  cap <- if (! is.na(cap <- caption(ht))) paste0('\\caption{', cap, '}\n') else ''
  lab <- if (! is.na(lab <- label(ht))) paste0('\\label{', lab, '}\n') else ''
  if (nzchar(lab) && ! nzchar(cap)) warning('No caption set: LaTeX table labels may not work as expected.')
  res <- if (caption_pos(ht) == 'top') paste0(cap, lab, res) else paste0(res, cap, lab)

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


huxtable_latex_dependencies <- list(
  rmarkdown::latex_dependency('array'),
  rmarkdown::latex_dependency('graphicx'),
  rmarkdown::latex_dependency('siunitx'),
  rmarkdown::latex_dependency('xcolor', options = 'table'),
  rmarkdown::latex_dependency('multirow'),
  rmarkdown::latex_dependency('hhline'),
  rmarkdown::latex_dependency('calc'),
  rmarkdown::latex_dependency('tabularx')
)

#' Report LaTeX Dependencies
#'
#' Prints out and returns a list of LaTeX dependencies for adding to a LaTeX preamble.
#'
#' @param quiet Suppress printing.
#'
#' @return A list of rmarkdown::latex_dependency objects, invisibly.
#' @export
#'
#' @examples
#' report_latex_dependencies()
#'
report_latex_dependencies <- function(quiet = FALSE) {
  if (! quiet) {
    report <- sapply(huxtable_latex_dependencies, function(ld) {
      str <- paste0('\\usepackage{', ld$name, '}')
      if (! is.null(ld$options)) {
        str <- paste0(str, '[', paste(ld$options, collapse = ','), ']')
      }
      paste0(str, '\n')
    })
    cat(paste0(report, collapse = ''))
    cat('% Other packages may be required if you use non-standard tabulars (e.g. tabulary)')
  }

  invisible((huxtable_latex_dependencies))
}

build_tabular <- function(ht) {

  res <- '\\let\\huxlen\\relax\n\\newlength\\huxlen\n' # for calculating lengths
  tenv <- tabular_environment(ht)
  res <- paste0(res, '\\begin{', tenv, '}')
  if (tenv %in% c('tabularx', 'tabular*', 'tabulary')) {
    tw <- width(ht)
    if (is.numeric(tw)) tw <- paste0(tw, '\\textwidth')
    res <- paste0(res, '{', tw,'}')
  }

  col_width <- col_width(ht)
  col_width <- if (all(is.na(col_width))) rep(1, ncol(ht)) else if (is.numeric(col_width)) col_width * ncol(ht) else
        col_width

  colspec <- character(ncol(ht))
  for (mycol in 1:ncol(ht)) {
    col_w <- col_width[mycol]

    hsize_redef <- if (! is.na(col_w) & is.numeric(col_w) & col_w != 1) paste0('\\hsize=', col_w, '\\hsize') else ''
    col_valign <- valign(ht)[,mycol]
    if (length(unique(col_valign)) > 1) warning(
      'In LaTeX, huxtable cannot currently use multiple vertical alignments in a single column.')
    col_valign <- col_valign[1]
    col_valign_str <- switch(as.character(col_valign), middle = 'm', bottom = 'b', top = , 'p')
    col_char <- if (is.na(col_w) || is.numeric(col_w)) 'X' else paste0(col_valign_str, '{', col_w, '}')
    if (col_valign != 'top' && col_char == 'X') warning(paste0(
          'In LaTex, huxtable cannot currently combine vertical alignment != "top" and proportional column widths. ',
          'Try specifying the column width in points ("100pt") or pixels ("50px").'))
    colspec[mycol] <- paste0('>{', hsize_redef, '}', col_char)

  }
  colspec <- paste0(colspec, collapse = ' ')
  colspec <- paste0('{', colspec, '}')
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

      contents <- ''
      rs <- dcell$rowspan
      end_row <- dcell$end_row
      bottom_left_multirow <- dcell$shadowed && myrow == end_row && mycol == dcol
      # STRATEGY:
      # - if not a shadowed cell, or if bottom left of a shadowed multirow,
      #    - print content, including struts for padding and row height
      #    - multirow goes upwards not downwards, to avoid content being overwritten by cell background
      # - if a left hand cell (shadowed or not), print cell color and borders
      if ((! dcell$shadowed && rs == 1) || bottom_left_multirow) {
        contents <- build_cell_contents(ht, drow, dcol)

        padding <- list(left_padding(ht)[drow, dcol], right_padding(ht)[drow, dcol], top_padding(ht)[drow, dcol],
              bottom_padding(ht)[drow, dcol])
        padding <- lapply(padding, function(x) if (is.numeric(x) & ! is.na(x)) paste0(x, 'pt') else x)
        hpadding <- lapply(padding[1:2], function(x) if (! is.na(x)) paste0('\\hspace*{', x ,'}') else '')
        contents <- paste0(hpadding[1], contents, hpadding[2])
        tpadding <- if (is.na(padding[3])) '' else paste0('\\rule{0pt}{\\baselineskip+', padding[3], '}')
        bpadding <- if (is.na(padding[4])) '' else paste0('\\rule[-', padding[4], ']{0pt}{', padding[4], '}')
        contents <- paste0(tpadding, contents, bpadding)
      }

      # to create row height, we add invisible \rule{0pt}. So, these heights are minimums.
      # not sure how this should interact with cell padding...
      if (! is.na(row_height <- row_height(ht)[drow])) {
        if (is.numeric(row_height)) row_height <- paste0(row_height, '\\textheight')
        contents <- paste0(contents, '\\rule{0pt}{', row_height, '}')
      }

      # print out cell_color and borders from display cell rather than actual cell
      # but only for left hand cells (which will be multicolumn{colspan} )
      if (! is.na(cell_color <- background_color(ht)[drow, dcol]) && mycol == dcol) {
        cell_color <- latex_color(cell_color)
        cell_color <- paste0('\\cellcolor[RGB]{', cell_color, '}')
        contents <- paste0(cell_color, ' ', contents)
      }

      if (bottom_left_multirow) {
        # the ctb switch may only work with v recent multirow
        # ctb <- switch(valign(ht)[myrow, mycol], top = 't', bottom = 'b', middle = 'c')
        # goes in [] as optional first argument
        # * is 'standard width', could be more specific:
        contents <- paste0('\\multirow{-', rs,'}{*}{', contents,'}')
      }

      if (mycol == dcol) {
        cs <- dcell$colspan
        lcr <- switch(align(ht)[drow, dcol], left   = 'l', right  = 'r', center = 'c')
        # pmb <- switch(valign(ht)[drow, dcol], top   = 'p', bottom  = 'b', center = 'm')
        # width_spec <- 'This is too hard if we have multicolumn cells...!'
        # align_str <- switch(align(ht)[drow, dcol], left   = '\\raggedright', right  = '\\raggedleft',
        #       center = '\\centering')
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
        # contents <- paste0('\\multicolumn{', cs,'}{', lb, pmb, width_spec, rb ,'}{', align_str, contents,'}')
      } # if (left cells of multicol or non-shadowed cell)

      row_contents[mycol] <- contents

    } # next cell
    row_contents <- row_contents[nzchar(row_contents)] # if we've printed nothing, don't print an & for it
    row_contents <- paste(row_contents, collapse = ' & ')
    # the 0.5pt avoids nasty pale lines through colored multirow cells
    res <- paste0(res, row_contents, ' \\tabularnewline[-0.5pt]\n')

    # add top/bottom borders
    res <- paste0(res, build_clines_for_row(ht, myrow))
  } # next row

  res <- paste0(res, '\\end{tabularx}\n')

  return(res)
}

build_cell_contents <- function(ht, row, col) {
  contents <- clean_contents(ht, row, col, type = 'latex')
  if (! is.na(font_size <- font_size(ht)[row, col])) {
    font_size_pt <- paste0(font_size, 'pt')
    line_space <- paste0(round(font_size * 1.2, 2), 'pt')
    contents <- paste0('{\\fontsize{', font_size_pt, '}{', line_space, '}\\selectfont ', contents, '}')
  }
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
  if (! is.na(font <- font(ht)[row, col])) {
    contents <- paste0('{\\fontfamily{', font, '}\\selectfont ', contents, '}')
  }
  if ((rt <- rotation(ht)[row, col]) != 0) {
    contents <- paste0('\\rotatebox{', rt, '}{', contents, '}')
  }

  return(contents)
}

build_clines_for_row <- function(ht, row) {
  # row can be from "0" for the top; up to nrow
  # add top/bottom borders
  # where a cell is shadowed, we don't want to add a top border (it'll go thru the middle)
  # bottom borders of a shadowed cell are fine, but come from the display cell.
  display_cells <- display_cells(ht)
  dcells_this_row <- unique(display_cells[display_cells$row == row,])
  this_bottom <- rep(0, ncol(ht))

  blank_line_color <- rep(latex_color('white'), ncol(ht)) # white by default, I guess...
  for (i in seq_len(nrow(dcells_this_row))) {
    drow <- dcells_this_row[i, 'display_row']
    dcol <- dcells_this_row[i, 'display_col']
    end_row <- dcells_this_row[i, 'end_row']
    end_col  <- dcells_this_row[i, 'end_col']
    # Print bottom border if we are at bottom of the display cell
    if (row == end_row) {
      bb <- bottom_border(ht)[drow, dcol]
      this_bottom[dcol:end_col] <- bb
    }
    # Use color if we are in middle of display cell
    if (row < end_row & ! is.na(color <- background_color(ht)[drow, dcol])) {
      blank_line_color[dcol:end_col] <- latex_color(color)
    }
  }
  blanks <- paste0('>{\\arrayrulecolor[RGB]{', blank_line_color ,'}}-')

  dcells_next_row <- unique(display_cells[display_cells$row == row + 1, ])
  next_top <- rep(0, ncol(ht))
  for (i in seq_len(nrow(dcells_next_row))) {
    drow <- dcells_next_row[i, 'display_row']
    dcol <- dcells_next_row[i, 'display_col']
    end_col <- dcells_next_row[i, 'end_col']
    # are we at the top of this dcell? If not...
    if (row + 1 != drow) next
    tb <- top_border(ht)[drow, dcol]
    next_top[dcol:end_col] <- tb
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
    right_col <- dcells_this_row[i, 'end_col'] - 1 # first rbs is always 0
    lbs[dcol] <- left_border(ht)[drow, dcol]
    rbs[right_col] <- right_border(ht)[drow, dcol]
  }

  pmax(lbs, rbs)
}

latex_color <- function (r_color) paste0(as.vector(col2rgb(r_color)), collapse = ', ')

