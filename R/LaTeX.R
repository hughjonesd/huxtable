

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
#' @return `to_latex` returns a string. `print_latex` prints the string and returns `NULL`.
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
  tabular <- build_tabular(ht)
  if (tabular_only) return(tabular)

  # table position
  pos_text <- switch(position(ht),
    left   = c('\\begin{raggedright}', '\\par\\end{raggedright}\n'),
    center = c('\\centering',   '\n'),
    right  = c('\\begin{raggedleft}',  '\\par\\end{raggedleft}\n')
  )

  resize_box <- if (is.na(height <- height(ht))) c('', '') else {
    if (is.numeric(height)) height <- sprintf('%.3g\\textheight', height)
    c(sprintf('\\resizebox*{!}{%s}{', height), '}')
  }

  cap <- if (is.na(cap <- caption(ht))) '' else {
    hpos <- get_caption_hpos(ht)
    cap_setup <- switch(hpos,
      left   = 'raggedright',
      center = 'centering',
      right  = 'raggedleft'
    )
    sprintf('\\captionsetup{justification=%s,singlelinecheck=off}\n\\caption{%s}\n', cap_setup, cap)
  }
  lab <- if (is.na(lab <- label(ht))) '' else sprintf('\\label{%s}\n', lab)
  if (nzchar(lab) && ! nzchar(cap)) warning('No caption set: LaTeX table labels may not work as expected.')
  res <- if (grepl('top', caption_pos(ht))) paste0(cap, lab, tabular) else paste0(tabular, cap, lab)

  begin_table <- sprintf('\\begin{table}[%s]\n', latex_float(ht))
  res <- paste0(begin_table, pos_text[1], resize_box[1], res,
        resize_box[2], pos_text[2], '\\end{table}\n')

  return(res)
}


huxtable_latex_dependencies <- list(
  list(name = 'array'),
  list(name = 'caption'),
  list(name = 'graphicx'),
  list(name = 'siunitx'),
  list(name = 'xcolor', options = 'table'),
  list(name = 'multirow'),
  list(name = 'hhline'),
  list(name = 'calc'),
  list(name = 'tabularx')
)

#' Report LaTeX dependencies
#'
#' Prints out and returns a list of LaTeX dependencies for adding to a LaTeX preamble.
#'
#' @param quiet Logical: suppress printing.
#' @param as_string Logical: return dependencies as a string.
#'
#' @return If `as_string` is `TRUE`, a string of "\\\\usepackage\\{...\\}" statements;
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

  if (as_string) {
    return(paste0(report, collapse = ''))
  } else {
    assert_package('report_latex_dependencies', 'rmarkdown')
    huxtable_latex_dependencies <- lapply(huxtable_latex_dependencies, function (x) {
      rmarkdown::latex_dependency(x$name, options = x$options)
    })
    return(invisible(huxtable_latex_dependencies))
  }
}


build_tabular <- function(ht) {
  if (! check_positive_dims(ht)) return('')
  colspec <- sapply(seq_len(ncol(ht)), function (mycol) paste0('p{', compute_width(ht, mycol, mycol), '}'))
  colspec <- paste0(colspec, collapse = ' ')
  res <- paste0('{', colspec, '}\n')

  display_cells     <- display_cells(ht, all = TRUE)
  dc_pos_matrix <- as.matrix(display_cells[, c('display_row', 'display_col')])
  all_contents      <- clean_contents(ht, type = 'latex')
  collapsed_borders <- collapsed_borders(ht)
  cb_colors         <- collapsed_border_colors(ht)

  ## CALCULATE HHLINES
  #  Done for n+1 rows including "row 0" at the top
  horiz_b <- collapsed_borders$horiz
  hb_maxes <- apply(horiz_b, 1, max)
  if (any(horiz_b > 0 & horiz_b < hb_maxes[row(horiz_b)])) warning(
        'Multiple horizontal border widths in a single row; using the maximum.')
  horiz_b <- ifelse(horiz_b == 0, horiz_b, hb_maxes[row(horiz_b)])
  hb_colors <- format_color(cb_colors$horiz, default = 'black')

  # background colors come from shadowing cells
  bg_colors <- background_color(ht)[dc_pos_matrix]
  bg_colors <- c(rep(NA, ncol(horiz_b)), bg_colors) # or, should color be taken from the row below?
  bg_colors <- format_color(bg_colors, default = 'white')
  hhline_colors <- ifelse(horiz_b > 0, hb_colors ,bg_colors)
  hhlines_horiz <- paste0('>{\\arrayrulecolor[RGB]{', hhline_colors, '}\\global\\arrayrulewidth=',
        horiz_b, 'pt}-')
  dim(hhlines_horiz) <- dim(horiz_b)
  no_hborder_in_row <- hb_maxes[row(hhlines_horiz)] == 0
  hhlines_horiz[no_hborder_in_row] <- ''

  vert_b <- collapsed_borders$vert # nrow X ncol + 1
  vert_b <- rbind(vert_b[1,], vert_b) # we checked positive dims; row 1 exists
  vert_bc <- cb_colors$vert
  vert_bc <- rbind(vert_bc[1,], vert_bc)
  vert_bc <- format_color(vert_bc)
  hhlines_vert <- rep('', length(vert_b))
  hhlines_vert <- ifelse(vert_b == 0, '', sprintf(
        '>{\\arrayrulecolor[RGB]{%s}\\global\\arrayrulewidth=%spt}|', vert_bc, vert_b))
  dim(hhlines_vert) <- c(nrow(horiz_b), ncol(horiz_b) + 1)

  # interleave vertical and horizontal lines like: |-|-|-|
  hhlines <- cbind(hhlines_horiz, hhlines_vert)
  hhlines <- matrix('', nrow(hhlines_horiz), ncol(hhlines_horiz) + ncol(hhlines_vert))
  hhlines[, seq(2, ncol(hhlines), 2)] <- hhlines_horiz
  hhlines[, seq(1, ncol(hhlines), 2)] <- hhlines_vert

  hhlines <- apply(hhlines, 1, paste0, collapse = '')
  hhlines <- sprintf('\n\n\\hhline{%s}\n\\arrayrulecolor{black}\n', hhlines)
  res <- paste0(res, hhlines[1])

  ## BUILD CELL CONTENTS
  fs <- font_size(ht)
  line_space <- round(fs * 1.2, 2)
  has_fs <- ! is.na(fs)
  all_contents[has_fs] <- sprintf('{\\fontsize{%.4gpt}{%.4gpt}\\selectfont %s}' ,
        fs[has_fs], line_space[has_fs], all_contents[has_fs])

  tc <- text_color(ht)
  tcf <- format_color(tc)
  has_tc <- ! is.na(tc)
  all_contents[has_tc] <- sprintf('\\textcolor[RGB]{%s}{%s}', tcf[has_tc], all_contents[has_tc])

  all_contents[bold(ht)]   <- sprintf('\\textbf{%s}', all_contents[bold(ht)])
  all_contents[italic(ht)] <- sprintf('\\textit{%s}', all_contents[italic(ht)])

  font <- font(ht)
  has_font <- ! is.na(font)
  all_contents[has_font] <- sprintf('{\\fontfamily{%s}\\selectfont %s}', font[has_font],
        all_contents[has_font])

  rt <- rotation(ht)
  has_rt <- rt != 0
  all_contents[has_rt] <- sprintf('\\rotatebox{%.4g}{%s}', rt[has_rt], all_contents[has_rt])

  dim(all_contents) <- dim(ht)

  real_align <- real_align(ht)
  ## UNREFORMED BIT
  ## contents are empty except for contents of the *bottom* left of a 'display area' (including 1x1)
  ##   - we can get an array of these with dcells[, c('display_col', 'end_row')] of unshadowed cells
  ##   - contents have padding, alignment, wrap and row_height TeX added
  ## cell colors and borders are added to left hand row of a 'display area'; these come
  ##   from the colors and borders of the 'display cell'
  ## 'multirow' is added to bottom left of a 'display area' if it has > 1 rowspan
  ##
  ## for each row:
  ##   get rid of empty cells
  ##   paste row together, collapsed with &; add '\\tabularnewline[-0.5pt]\n'
  ##
  ## add hhlines below each row (including "row 0")
  for (myrow in seq_len(nrow(ht))) {
    row_contents <- character(0)
    added_right_border <- FALSE

    for (mycol in seq_len(ncol(ht))) {
      dcell <- display_cells[myrow + (mycol - 1) * nrow(ht), ] # speed
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
        contents <- all_contents[drow, dcol]

        padding <- list(left_padding(ht)[drow, dcol], right_padding(ht)[drow, dcol], top_padding(ht)[drow, dcol],
              bottom_padding(ht)[drow, dcol])
        padding <- lapply(padding, function(x) if (is_a_number(x)) paste0(x, 'pt') else x)
        tpadding <- if (is.na(padding[3])) '' else paste0('\\rule{0pt}{\\baselineskip+', padding[3], '}')
        bpadding <- if (is.na(padding[4])) '' else paste0('\\rule[-', padding[4], ']{0pt}{', padding[4], '}')
        align_str <- switch(real_align[drow, dcol],
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
        contents <- paste0(cell_color, '', contents)
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
          switch(real_align[drow, dcol], left = 'l', center = 'c', right = 'r')
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
    res <- paste0(res, hhlines[1 + myrow])
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


# uses "real" border numbers in "ncol + 1 space"
v_border <- function (ht, row, col, collapsed_borders, cb_colors) {
  width <- collapsed_borders$vert[row, col]
  color <- cb_colors$vert[row, col]
  color <- format_color(color, default = 'black')

  paste0('!{\\color[RGB]{', color, '}\\vrule width ', width, 'pt}')
}
