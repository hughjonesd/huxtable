

#' @import assertthat
NULL


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
  assert_that(is.flag(tabular_only))
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
    paste0('\\captionsetup{justification=', cap_setup, ',singlelinecheck=off}\n\\caption{', cap, '}\n')
  } else ''
  lab <- if (is.na(lab <- label(ht))) '' else paste0('\\label{', lab, '}\n')
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
#' @param quiet Logical: suppress printing.
#' @param as_string Logical: return dependencies as a string.
#'
#' @return If \code{as_string} is \code{TRUE}, a string of "\\usepackage{...}" statements;
#'   otherwise a list of rmarkdown::latex_dependency objects, invisibly.
#' @export
#'
#' @examples
#' report_latex_dependencies()
#'
report_latex_dependencies <- function(quiet = FALSE, as_string = FALSE) {
  assert_that(is.flag(quiet), is.flag(as_string))

  report <- sapply(huxtable_latex_dependencies, function(ld) {
    package_str <- '\\usepackage'
    if (! is.null(ld$options)) {
      options_str <- paste(ld$options, collapse = ',')
      package_str <- paste0(package_str, '[', options_str, ']')
    }
    package_str <- paste0(package_str, '{', ld$name, '}\n')
    package_str
  })
  if (! quiet) {
    cat(paste0(report, collapse = ''))
    cat('% Other packages may be required if you use non-standard tabulars (e.g. tabulary)')
  }

  if (as_string) paste0(report, collapse = '') else invisible(huxtable_latex_dependencies)
}


build_tabular <- function(ht) {
  colspec <- sapply(seq_len(ncol(ht)), function (mycol) paste0('p{', compute_width(ht, mycol, mycol), '}'))
  colspec <- paste0(colspec, collapse = ' ')
  res <- paste0('{', colspec, '}\n')

  display_cells     <- display_cells(ht, all = TRUE)
  all_contents      <- clean_contents(ht, type = 'latex')
  collapsed_borders <- collapsed_borders(ht)
  cb_colors         <- collapsed_border_colors(ht)
  res <- paste0(res, build_clines_for_row(ht, row = 0, collapsed_borders, cb_colors))

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
        hpadding <- lapply(padding[1:2], function (x) if (is.na(x)) '' else paste0('\\hspace*{', x, '}'))
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
        lb <- if (! added_right_border) v_border(ht, myrow, mycol, collapsed_borders, cb_colors) else ''
        rb <- v_border(ht, myrow, dcell$end_col + 1, collapsed_borders, cb_colors) # we need to put the end column here
        added_right_border <- rb != ''
        contents <- paste0('\\multicolumn{', cs, '}{', lb, colspec, rb, '}{', contents, '}')
      }

      row_contents[mycol] <- contents

    } # next cell
    row_contents <- row_contents[nzchar(row_contents)] # if we've printed nothing, don't print an & for it
    row_contents <- paste(row_contents, collapse = ' & \n')
    # should reduce nasty pale lines through colored multirow cells:
    res <- paste0(res, row_contents, '\\tabularnewline[-0.5pt]\n')

    # add top/bottom borders
    res <- paste0(res, build_clines_for_row(ht, myrow, collapsed_borders, cb_colors))
  } # next row

  tenv <- tabular_environment(ht)
  width_spec <- if (tenv %in% c('tabularx', 'tabular*', 'tabulary')) {
    tw <- width(ht)
    if (is_a_number(tw)) tw <- paste0(tw, default_table_width_unit)
    paste0('{', tw, '}')
  } else {
    ''
  }
  res <- paste0('\\begin{',  tenv, '}', width_spec, res, '\\end{', tenv, '}\n')

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
    extra_seps <- (end_col - start_col) * 2
    cw <- paste0(cw, '+', extra_seps, '\\tabcolsep')
  }

  cw
}

build_cell_contents <- function(ht, row, col, contents) {
  if (! is.na(font_size <- font_size(ht)[row, col])) {
    line_space <- round(font_size * 1.2, 2)
    contents <- paste0('{\\fontsize{', font_size, 'pt}{', line_space, 'pt}\\selectfont ', contents, '}')
  }
  if (! is.na(text_color <- text_color(ht)[row, col])) {
    text_color <- format_color(text_color)
    contents <- paste0('\\textcolor[RGB]{', text_color, '}{', contents, '}')
  }
  if (bold(ht)[row, col])   contents <- paste0('\\textbf{', contents, '}')
  if (italic(ht)[row, col]) contents <- paste0('\\textit{', contents, '}')
  if (! is.na(font <- font(ht)[row, col])) {
    contents <- paste0('{\\fontfamily{', font, '}\\selectfont ', contents, '}')
  }
  if ( (rt <- rotation(ht)[row, col]) != 0) contents <- paste0('\\rotatebox{', rt, '}{', contents, '}')

  return(contents)
}

# row can be from "0" for the top; up to nrow
build_clines_for_row <- function(ht, row, collapsed_borders, cb_colors) {
  # where a cell is shadowed, we don't want to add a top border (it'll go thru the middle)
  # bottom borders of a shadowed cell are fine, but come from the display cell.
  display_cells <- display_cells(ht, all = TRUE)
  display_cells <- display_cells[display_cells$row == row, ]

  blank_line_color <- rep('white', ncol(ht)) # never now used
  for (i in seq_len(nrow(display_cells))) {
    dc <- display_cells[i, ]
    # Use color if we are in middle of display cell, also if we are at end and after a +ve border
      blank_line_color[dc$display_col:dc$end_col] <- background_color(ht)[dc$display_row, dc$display_col]
  }

  widths <- collapsed_borders$horiz[row + 1, ]
  if (all(widths == 0)) {
    return('')
  } else {
    width <- max(widths)
    if (! all(widths[widths > 0] == width)) warning("Multiple widths in a single border, using max")
    colors <- cb_colors$horiz[row + 1, ]
    colors <- sapply(colors, format_color, default = 'black')
    blank_line_color <- sapply(blank_line_color, format_color, default = 'white')
    hhlinechars <- sapply(seq_along(widths), function (x) {
      col <- if (widths[x] > 0) colors[x] else blank_line_color[x]
      paste0('>{\\arrayrulecolor[RGB]{', col, '}\\global\\arrayrulewidth=', width, 'pt}-')
    })
    vertlines <- compute_vertical_borders(ht, row, collapsed_borders, cb_colors)

    hhline <- paste0(hhlinechars, vertlines[-1], collapse = '')
    hhline <- paste0(vertlines[1], hhline)
    hhline <- paste0('\n\n\\hhline{', hhline, '}\n\\arrayrulecolor{black}\n')
    return(hhline)
  }
}


# these are inserted into the hhline. They have to have the same arrayrulewidth as the
# left/right borders of the cell above and below. That is, arrayrulewidth = max(right of above_left,
# left of above_right, right of below_left, left of below_right).
# row can be 0 for the top line.
# returns an ncol(ht) + 1 string array
compute_vertical_borders <- function (ht, row, collapsed_borders, cb_colors) {
  # if we are at the top line, then we'll assume we want the same vertical borders as on the first line below.
  if (row == 0) row <- 1
  b_widths <- collapsed_borders$vert[row, ]
  b_cols   <- cb_colors$vert[row, ]
  borders <- sapply(seq_along(b_widths), function (x){
    if (b_widths[x] == 0 ) return('')
    my_col <- format_color(b_cols[x], default = 'black')
    paste0('>{\\arrayrulecolor[RGB]{', my_col, '}\\global\\arrayrulewidth=', b_widths[x], 'pt}|')
  })

  return(borders)
}

# uses "real" border numbers in "ncol + 1 space"
v_border <- function (ht, row, col, collapsed_borders, cb_colors) {
  width <- collapsed_borders$vert[row, col]
  color <- cb_colors$vert[row, col]
  color <- format_color(color, default = 'black')

  paste0('!{\\color[RGB]{', color, '}\\vrule width ', width, 'pt}')
}
