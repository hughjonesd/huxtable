

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
  res <- '\\begingroup'

  res <- paste0(res, '\\begin{table}[h]\n')
  pos_text <- switch(position(ht),
        left   = c('\\begin{raggedright}', '\\par\\end{raggedright}'),
        center = c('\\begin{centering}',   '\\par\\end{centering}'),
        right  = c('\\begin{raggedleft}',  '\\par\\end{raggedleft}')
  )
  res <- paste0(res, pos_text[1], '\n')
  if (! is.na(cap <- caption(ht)) & caption_pos(ht) == 'top') {
    res <- paste0(res, '\\caption{', cap, '}\n')
  }
  # convenience function
  # don't indent or pandoc may treat it as verbatim
  res <- paste0(res, '
\\let\\Oldarrayrulewidth\\relax
\\newlength\\Oldarrayrulewidth
\\providecommand{\\Cline}[2]{}
\\renewcommand{\\Cline}[2]{%
\\noalign{\\global\\setlength{\\Oldarrayrulewidth}{\\arrayrulewidth}}%
\\noalign{\\global\\setlength{\\arrayrulewidth}{#1}}\\cline{#2}%
\\noalign{\\global\\setlength{\\arrayrulewidth}{\\Oldarrayrulewidth}}}
')
  res <- paste0(res, '\\begin{tabularx}')
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

  cell_shadows_row <- cell_shadows(ht, 'row')
  cell_shadows_col <- cell_shadows(ht, 'col')

  borders <- top_border(ht)[1,]
  res <- paste0(res, make_clines(borders))
  for (myrow in 1:nrow(ht)) {
    for (mycol in 1:ncol(ht)) {
      if (cell_shadows_col[myrow, mycol]) next
      if (! cell_shadows_row[myrow, mycol]) {

        contents <- clean_contents(ht, myrow, mycol, type = 'latex')

        if (! is.na(cell_color <- bgcolor(ht)[myrow, mycol])) {
          cell_color <- as.vector(col2rgb(cell_color))
          cell_color <- paste0(cell_color, collapse = ', ')
          cell_color <- paste0('\\cellcolor[RGB]{', cell_color, '}')
          contents <- paste0(cell_color, ' ', contents)
        }

        if (! is.na(text_color <- text_color(ht)[myrow, mycol])) {
          text_color <- as.vector(col2rgb(text_color))
          text_color <- paste0(text_color, collapse = ', ')
          contents <- paste0('\\textcolor[RGB]{', text_color, '}{', contents,'}')
        }

        if (bold(ht)[myrow, mycol]) {
          contents <- paste0('\\textbf{', contents, '}')
        }

        if (italic(ht)[myrow, mycol]) {
          contents <- paste0('\\textit{', contents, '}')
        }

        if ((rs <- rowspan(ht)[myrow, mycol]) > 1) {
          # the ctb switch may only work with v recent multirow
          # ctb <- switch(valign(ht)[myrow, mycol],
          #       top    = 't',
          #       bottom = 'b',
          #       middle = ,
          #       'c')
          # * is width, could be more specific
          contents <- paste0('\\multirow{', rs,'}{*}{', contents,'}')
        }

        # must be this way round: multirow inside multicolumn
        # always use multicolumn so as to allow vertical borders for specific cells
        cs <- colspan(ht)[myrow, mycol]
        lcr <- switch(align(ht)[myrow, mycol],
              left   = 'l',
              right  = 'r',
              center = ,
              'c')
        if (left_border(ht)[myrow, mycol] > 0) lcr <- paste0('|', lcr)
        if (right_border(ht)[myrow, mycol] > 0) lcr <- paste0(lcr, '|')
#        contents <- paste0('\\begingroup\\setlength\\tabcolsep{6pt}', contents, '\\endgroup')
        contents <- paste0('\\multicolumn{', cs,'}{', lcr ,'}{', contents,'}')
        res <- paste0(res, contents)
      } # end 'if cell is unshadowed'

      real_col <- sum(colspan(ht)[myrow, 1:mycol]) # but will this fail when we have multirows?
      if (real_col < ncol(ht)) res <- paste0(res, ' & ')
    } # next cell
    res <- paste0(res, ' \\tabularnewline\n')

    # add top/bottom borders
    this_bottom <- bottom_border(ht)[myrow,]
    next_top <- if (myrow < nrow(ht)) top_border(ht)[myrow + 1,] else 0
    borders <- pmax(this_bottom, next_top)
    # don't print borders before a shadowed row:
    if (myrow < nrow(ht)) borders[cell_shadows_row[myrow + 1,]] <- 0
    if (any(borders > 0)) {
      res <- paste0(res, make_clines(borders))
    }
  } # next row

  res <- paste0(res, '\\end{tabularx}\n')
  if (! is.na(cap <- caption(ht)) & caption_pos(ht) == 'bottom') {
    res <- paste0(res, '\\caption{', cap, '}\n')
  }
  res <- paste0(res, pos_text[2], '\n') # table positioning
  res <- paste0(res, '\\end{table}\n')
  res <- paste0(res, '\\endgroup\n')
  res
}

make_clines <- function(borders) {
  cells <- seq_along(borders)
  cells <- cells[borders > 0]
  borders <- paste0(borders, 'pt')
  paste0('\\Cline{', borders,'}{', cells, '-', cells, '}\n', collapse=' ')
  #paste0('\\cline{', cells, '-', cells, '}\n', collapse=' ')
}

#' @export
#'
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}
