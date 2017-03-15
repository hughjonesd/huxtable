

default_table_width_unit <- '\\textwidth'


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

  cap <- if (! is.na(cap <- caption(ht))) {
    cap_setup <- switch(position(ht),
            left   = 'raggedright',
            center = 'centering',
            right  = 'raggedleft'
          )
    cap_setup <- paste0('\\captionsetup{justification=', cap_setup,',singlelinecheck=off}\n')
    paste0(cap_setup, '\\caption{', cap, '}\n')
  } else ''
  lab <- if (! is.na(lab <- label(ht))) paste0('\\label{', lab, '}\n') else ''
  if (nzchar(lab) && ! nzchar(cap)) warning('No caption set: LaTeX table labels may not work as expected.')
  res <- if (caption_pos(ht) == 'top') paste0(cap, lab, res) else paste0(res, cap, lab)

  # table position
  pos_text <- switch(position(ht),
    left   = c('\\begin{raggedright}', '\\par\\end{raggedright}'),
    center = c('\\centering',   ''),
    right  = c('\\begin{raggedleft}',  '\\par\\end{raggedleft}')
  )
  res <- paste0(pos_text[1], res, pos_text[2], '\n')

  res <- paste0('\\begin{table}[h]\n', res, '\\end{table}\n')

  return(res)
}


huxtable_latex_dependencies <- list(
  rmarkdown::latex_dependency('array'),
  rmarkdown::latex_dependency('caption'),
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
  col_width <- col_width(ht)
  if (all(is.na(col_width))) col_width <- rep(1/ncol(ht), ncol(ht))
  col_width[is_a_number(col_width)] <- as.numeric(col_width[is_a_number(col_width)]) * ncol(ht)
  colspec <- character(ncol(ht))
  for (mycol in 1:ncol(ht)) {
    # col type will be redefined when valign is different:
    colspec[mycol] <- paste0('p{', compute_width(ht, mycol, mycol), '}')
  }
  colspec <- paste0(colspec, collapse = ' ')
  colspec <- paste0('{', colspec, '}')
  res <- paste0(colspec, '\n')

  display_cells <- display_cells(ht)
  all_contents  <- clean_contents(ht, type = 'latex')
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
        contents <- build_cell_contents(ht, drow, dcol, all_contents[drow, dcol])

        padding <- list(left_padding(ht)[drow, dcol], right_padding(ht)[drow, dcol], top_padding(ht)[drow, dcol],
              bottom_padding(ht)[drow, dcol])
        padding <- lapply(padding, function(x) if (is_a_number(x)) paste0(x, 'pt') else x)
        tpadding <- if (is.na(padding[3])) '' else paste0('\\rule{0pt}{\\baselineskip+', padding[3], '}')
        bpadding <- if (is.na(padding[4])) '' else paste0('\\rule[-', padding[4], ']{0pt}{', padding[4], '}')
        contents <- paste0(tpadding, contents, bpadding)
        if (wrap(ht)[drow, dcol]) {
          width_spec <- compute_width(ht, mycol, dcell$end_col)
          hpad_loss  <- lapply(padding[1:2], function (x) if (! is.na(x)) paste0('-',x) else '')
          align_str <- switch(align(ht)[drow, dcol],
                  left   = '\\raggedright',
                  right  = '\\raggedleft',
                  center = '\\centering'
                )
          contents   <- paste0('\\parbox[c]{', width_spec , hpad_loss[1], hpad_loss[2], '}{', align_str, contents, '}')
        }
        hpadding <- lapply(padding[1:2], function (x) if (! is.na(x)) paste0('\\hspace*{', x ,'}') else '')
        contents <- paste0(hpadding[1], contents, hpadding[2])

        # to create row height, we add invisible \rule{0pt}. So, these heights are minimums.
        # not sure how this should interact with cell padding...
        if (! is.na(row_height <- row_height(ht)[drow])) {
          if (is.numeric(row_height)) row_height <- paste0(row_height, '\\textheight')
          contents <- paste0(contents, '\\rule{0pt}{', row_height, '}')
        }
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

      if (mycol == dcol) { # first column of cell
        cs <- dcell$colspan
        pmb <- switch(valign(ht)[drow, dcol], top   = 'p', bottom  = 'b', middle = 'm')
        width_spec <- compute_width(ht, mycol, dcell$end_col)
        colspec <- paste0(pmb, '{', width_spec, '}')
        # only add left borders if we haven't already added a right border!
        lb <- if (left_border(ht)[drow, dcol] > 0 && ! added_right_border) '|' else ''
        rb <- if ((added_right_border <- right_border(ht)[drow, dcol]) > 0) '|' else ''
        contents <- paste0('\\multicolumn{', cs,'}{', lb, colspec, rb ,'}{', contents,'}')
      } # if (first column of cell)

      row_contents[mycol] <- contents

    } # next cell
    row_contents <- row_contents[nzchar(row_contents)] # if we've printed nothing, don't print an & for it
    row_contents <- paste(row_contents, collapse = ' & ')
    # the 0.5pt avoids nasty pale lines through colored multirow cells
    res <- paste0(res, row_contents, ' \\tabularnewline[-0.5pt]\n')

    # add top/bottom borders
    res <- paste0(res, build_clines_for_row(ht, myrow))
  } # next row

  tenv <- tabular_environment(ht)
  width_spec <- if (tenv %in% c('tabularx', 'tabular*', 'tabulary')) {
    tw <- width(ht)
    if (is.numeric(tw)) tw <- paste0(tw, default_table_width_unit)
    paste0('{', tw,'}')
  } else {
    ''
  }
  res <- paste0('\\begin{', tenv, '}', width_spec, res, '\\end{', tenv, '}\n')

  return(res)
}

compute_width <- function(ht, start_col, end_col) {
  table_width <- width(ht) # always defined, default is 0.5 (of \\textwidth)
  if (! is.numeric(table_width)) {
    table_unit  <- gsub('\\d', '', table_width)
    table_width <- as.numeric(gsub('\\D', '', table_width))
  } else {
    table_unit <- default_table_width_unit
  }

  cw <- col_width(ht)[start_col:end_col]
  if (! all(is_a_number(cw))) {
    # use calc for multiple character widths
    # won't work if you mix in numerics
    cw[is.na(cw)] <- paste0(1/ncol(ht), table_unit)
    cw <- paste(cw, collapse = '+')
  } else {
    cw[is.na(cw)] <- 1/ncol(ht)
    cw <- sum(as.numeric(cw))
    cw <- cw * table_width
    cw <- paste0(cw, table_unit)
  }

  if (end_col > start_col) {
    # need to add some extra tabcolseps, one per column
    cw <- paste0(cw, '+', (end_col - start_col) * 2, '\\tabcolsep')
  }

  cw
}

build_cell_contents <- function(ht, row, col, contents) {
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

