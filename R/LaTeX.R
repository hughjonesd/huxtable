

default_table_width_unit <- '\\textwidth'


#' @export
#'
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}


#' Create LaTeX representing a huxtable
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
    hpos <- sub('.*(left|center|right)', '\\1', caption_pos(ht))
    if (! hpos %in% c('left', 'center', 'right')) hpos <- position(ht)
    cap_setup <- switch(hpos,
            left   = 'raggedright',
            center = 'centering',
            right  = 'raggedleft'
          )
    cap_setup <- paste0('\\captionsetup{justification=', cap_setup, ',singlelinecheck=off}\n')
    paste0(cap_setup, '\\caption{', cap, '}\n')
  } else ''
  lab <- if (! is.na(lab <- label(ht))) paste0('\\label{', lab, '}\n') else ''
  if (nzchar(lab) && ! nzchar(cap)) warning('No caption set: LaTeX table labels may not work as expected.')
  res <- if (grepl('top', caption_pos(ht))) paste0(cap, lab, res) else paste0(res, cap, lab)

  # table position
  pos_text <- switch(position(ht),
    left   = c('\\begin{raggedright}', '\\par\\end{raggedright}'),
    center = c('\\centering',   ''),
    right  = c('\\begin{raggedleft}',  '\\par\\end{raggedleft}')
  )
  res <- paste0(pos_text[1], res, pos_text[2], '\n')

  res <- paste0('\\begin{table}[', latex_float(ht), ']\n', res, '\\end{table}\n')

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

#' Report LaTeX dependencies
#'
#' Prints out and returns a list of LaTeX dependencies for adding to a LaTeX preamble.
#'
#' @param quiet Suppress printing.
#' @param as_string Return dependencies as a string.
#'
#' @return If \code{as_string} is \code{TRUE}, a string of "\\usepackage{...}" statements;
#'   otherwise a list of rmarkdown::latex_dependency objects, invisibly.
#' @export
#'
#' @examples
#' report_latex_dependencies()
#'
report_latex_dependencies <- function(quiet = FALSE, as_string = FALSE) {
  report <- sapply(huxtable_latex_dependencies, function(ld) {
    str <- '\\usepackage'
    if (! is.null(ld$options)) {
      str <- paste0(str, '[', paste(ld$options, collapse = ','), ']')
    }
    str <- paste0(str, '{', ld$name, '}\n')
    str
  })
  if (! quiet) {
    cat(paste0(report, collapse = ''))
    cat('% Other packages may be required if you use non-standard tabulars (e.g. tabulary)')
  }

  if (as_string) paste0(report, collapse = '') else invisible(huxtable_latex_dependencies)
}


build_tabular <- function(ht) {
  colspec <- character(ncol(ht))
  for (mycol in 1:ncol(ht)) {
    # col type will be redefined when valign is different:
    colspec[mycol] <- paste0('p{', compute_width(ht, mycol, mycol), '}')
  }
  colspec <- paste0(colspec, collapse = ' ')
  colspec <- paste0('{', colspec, '}')
  res <- paste0(colspec, '\n')

  display_cells <- display_cells(ht, all = TRUE)
  all_contents  <- clean_contents(ht, type = 'latex')
  res <- paste0(res, build_clines_for_row(ht, row = 0))

  for (myrow in 1:nrow(ht)) {
    row_contents <- character(0)
    added_right_border <- FALSE

    for (mycol in 1:ncol(ht)) {
      dcell <- display_cells[display_cells$row == myrow & display_cells$col == mycol, ]
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
      if ( (! dcell$shadowed && rs == 1) || bottom_left_multirow) {
        contents <- build_cell_contents(ht, drow, dcol, all_contents[drow, dcol])

        padding <- list(left_padding(ht)[drow, dcol], right_padding(ht)[drow, dcol], top_padding(ht)[drow, dcol],
              bottom_padding(ht)[drow, dcol])
        padding <- lapply(padding, function(x) if (is_a_number(x)) paste0(x, 'pt') else x)
        tpadding <- if (is.na(padding[3])) '' else paste0('\\rule{0pt}{\\baselineskip+', padding[3], '}')
        bpadding <- if (is.na(padding[4])) '' else paste0('\\rule[-', padding[4], ']{0pt}{', padding[4], '}')
        align_str <- switch(align(ht)[drow, dcol],
          left   = '\\raggedright ',
          right  = '\\raggedleft ',
          center = '\\centering '
        )
        contents <- paste0(tpadding, align_str, contents, bpadding)
        if (wrap(ht)[drow, dcol]) {
          width_spec <- compute_width(ht, mycol, dcell$end_col)
          hpad_loss  <- lapply(padding[1:2], function (x) if (! is.na(x)) paste0('-', x) else '')
          # reverse of what you think. 'b' aligns the *bottom* of the text with the baseline
          # this doesn't really work for short text!
          ctb <- switch(valign(ht)[drow, dcol], top = 'b', middle = 'c', bottom = 't')
          contents   <- paste0('\\parbox[', ctb, ']{', width_spec, hpad_loss[1], hpad_loss[2], '}{', contents, '}')
        }
        hpadding <- lapply(padding[1:2], function (x) if (! is.na(x)) paste0('\\hspace*{', x, '}') else '')
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
        cell_color <- format_color(cell_color)
        cell_color <- paste0('\\cellcolor[RGB]{', cell_color, '}')
        contents <- paste0(cell_color, ' ', contents)
      }

      if (bottom_left_multirow) {
        # * is 'standard width', could be more specific:
        contents <- paste0('\\multirow{-', rs, '}{*}{', contents, '}')
      }

      if (mycol == dcol) { # first column of cell
        cs <- dcell$colspan
        colspec <- if (wrap(ht)[drow, dcol]) {
          pmb <- switch(valign(ht)[drow, dcol], top   = 'p', bottom  = 'b', middle = 'm')
          width_spec <- compute_width(ht, mycol, dcell$end_col)
          paste0(pmb, '{', width_spec, '}')
        } else {
          switch(align(ht)[drow, dcol], left = 'l', center = 'c', right = 'r')
        }
        # only add left borders if we haven't already added a right border!
        lb <- if (! added_right_border) v_border(ht, myrow, mycol, 'left') else ''
        rb <- v_border(ht, myrow, mycol, 'right')
        added_right_border <- rb != ''
        contents <- paste0('\\multicolumn{', cs, '}{', lb, colspec, rb, '}{', contents, '}')
      }

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
    if (is_a_number(tw)) tw <- paste0(tw, default_table_width_unit)
    paste0('{', tw, '}')
  } else {
    ''
  }
  res <- paste0('\\begin{', tenv, '}', width_spec, res, '\\end{', tenv, '}\n')

  return(res)
}


compute_width <- function (ht, start_col, end_col) {
  table_width <- width(ht) # always defined, default is 0.5 (of \\textwidth)
  if (is_a_number(table_width)) {
    table_unit  <- default_table_width_unit
    table_width <- as.numeric(table_width)
  } else {
    table_unit  <- gsub('\\d', '', table_width)
    table_width <- as.numeric(gsub('\\D', '', table_width))
  }

  cw <- col_width(ht)[start_col:end_col]
  cw[is.na(cw)] <- 1 / ncol(ht)
  if (! all(nums <- is_a_number(cw))) {
    # use calc for multiple character widths
    # won't work if you mix in numerics
    cw[nums] <- paste0(as.numeric(cw[nums]) * table_width, table_unit)
    cw <- paste(cw, collapse = '+')
  } else {
    cw <- sum(as.numeric(cw))
    cw <- cw * table_width
    cw <- paste0(cw, table_unit)
  }

  if (end_col > start_col) {
    # need to add some extra tabcolseps, two per column
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
    text_color <- format_color(text_color)
    contents <- paste0('\\textcolor[RGB]{', text_color, '}{', contents, '}')
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
  if ( (rt <- rotation(ht)[row, col]) != 0) {
    contents <- paste0('\\rotatebox{', rt, '}{', contents, '}')
  }

  return(contents)
}

build_clines_for_row <- function(ht, row) {
  # row can be from "0" for the top; up to nrow
  # add top/bottom borders
  # where a cell is shadowed, we don't want to add a top border (it'll go thru the middle)
  # bottom borders of a shadowed cell are fine, but come from the display cell.
  display_cells <- display_cells(ht, all = TRUE)
  dcells_this_row <- unique(display_cells[display_cells$row == row, ])
  this_bottom <- rep('', ncol(ht))

  blank_line_color <- rep(format_color('white'), ncol(ht)) # white by default, I guess...
  # let's figure out the vertical line width right now, as the maximum of all non shadowed cells
  # that border this row
  top_cells <- display_cells[display_cells$end_row == row & ! display_cells$shadowed,]
  bottom_cells <- display_cells[display_cells$display_row == row + 1 & ! display_cells$shadowed,]
  top_widths <- bottom_border(ht)[top_cells$display_row, top_cells$display_col]
  bottom_widths <- top_border(ht)[bottom_cells$display_row, bottom_cells$display_col]
  for (vec in list(top_widths, bottom_widths)) if (length(unique(vec[vec > 0])) > 1) warning(
          'LaTeX cannot deal with multiple border widths in a row. Using the maximum width')
  width <- max(top_widths, bottom_widths, 0) # avoid "no non-missing" complaint
  for (i in seq_len(nrow(dcells_this_row))) {
    drow <- dcells_this_row[i, 'display_row']
    dcol <- dcells_this_row[i, 'display_col']
    end_row <- dcells_this_row[i, 'end_row']
    end_col  <- dcells_this_row[i, 'end_col']
    # Print bottom border if we are at bottom of the display cell
    if (row == end_row) {
      this_bottom[dcol:end_col] <- h_border(ht, drow, dcol, 'bottom', width)
    }
    # Use color if we are in middle of display cell
    if (row < end_row & ! is.na(color <- background_color(ht)[drow, dcol])) {
      blank_line_color[dcol:end_col] <- format_color(color)
    }
  }

  dcells_next_row <- unique(display_cells[display_cells$row == row + 1, ])
  next_top <- rep('', ncol(ht))
  for (i in seq_len(nrow(dcells_next_row))) {
    drow <- dcells_next_row[i, 'display_row']
    dcol <- dcells_next_row[i, 'display_col']
    end_col <- dcells_next_row[i, 'end_col']
    # are we at the top of this dcell? If not...
    if (row + 1 != drow) next
    next_top[dcol:end_col] <- h_border(ht, drow, dcol, 'top', width)
  }
  borders <- this_bottom
  borders[borders == ''] <- next_top[borders == '']

  blanks <- paste0('>{\\arrayrulecolor[RGB]{', blank_line_color, '}\\global\\arrayrulewidth=', width, 'pt}-')
  if (any(borders != '')) {
    hhlinechars <- ifelse(borders != '', borders, blanks)
    vertlines <- compute_vertical_borders(ht, row)
    hhlinechars <- paste0(hhlinechars, vertlines[-1], collapse = '')
    hhlinechars <- paste0(vertlines[1], hhlinechars)
    hhline <- paste0('\\hhline{', hhlinechars, '}\n')
    hhline <- paste0(hhline, '\\arrayrulecolor{black}\n') # don't let arrayrulecolor spill over

    return(hhline)
  } else {
    return('')
  }
}


# these are inserted into the hhline. They have to have the same arrayrulewidth as the
# left/right borders of the cell above and below. That is, arrayrulewidth = max(right of above_left,
# left of above_right, right of below_left, left of below_right).
# row can be 0 for the top line.
# returns an ncol(ht) + 1 string array
compute_vertical_borders <- function (ht, row) {
  # if we are at the top line, then we'll assume we want the same vertical borders as on the first line below.
  if (row == 0) row <- 1
  b_widths <- collapsed_borders(ht)$vert[row, ]
  b_cols   <- collapsed_border_colors(ht)$vert[row, ]
  borders <- sapply(seq_along(b_widths), function (x){
    if (b_widths[x] == 0 ) return('')
    my_col <- format_color(b_cols[x], default = 'black')
    tex_glue('>{\\arrayrulecolor[RGB]{<< my_col >>}\\global\\arrayrulewidth=<< b_widths[x] >>pt}|')
  })

  return(borders)
}



v_border <- function (ht, row, col, side) {
  if (side == 'right') col <- col + 1
  width <- collapsed_borders(ht)$vert[row, col]
  color <- collapsed_border_colors(ht)$vert[row, col]
  color <- format_color(color, default = 'black')

  tex_glue('!{\\color[RGB]{<< color >>}\\vrule width << width >>pt}')
}


h_border <- function (ht, drow, dcol, side, width) {
  # width <- get_all_borders(ht, drow, dcol)[side] # don't use this as we can't
  if (! width > 0 ) return('')
  color <- get_all_border_colors(ht, drow, dcol)[side]
  color <- format_color(color, default = 'black')
  paste0('>{\\arrayrulecolor[RGB]{', color, '}\\global\\arrayrulewidth=', width, 'pt}-')
}

# this has to have the ... argument as it's (???) evaluated in the parent frame
tex_glue <- function (...) glue::glue(..., .open = '<<', .close = '>>', .envir = parent.frame())
