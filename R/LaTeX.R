

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
  # \begin{tabular}{ l | c || r | }
  # \hline
  # 1 & 2 & 3 \\
  # 4 & 5 & 6 \\
  # 7 & 8 & 9 \\
  # \hline
  # \end{tabular}
  res <- '\\begin{tabularx}'
  tw <- width(ht)
  if (is.numeric(tw)) tw <- paste0(tw, '\\textwidth')
  res <- paste0(res, '{', tw,'}')
  col_widths <- col_widths(ht)
  if (all(is.na(col_widths))) col_widths <- rep(1/ncol(ht), ncol(ht))
  if (is.numeric(col_widths)) {
    col_widths <- paste0(col_widths, '\\textwidth')
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
    col_width <- col_widths[mycol]
    colspec_valign_str <- paste0(colspec_valign_str, '{', col_width,  '}')
    colspec <- paste0(colspec, colspec_align_str, colspec_valign_str, ' ')
  }
  colspec <- paste0(colspec, '}')
  res <- paste0(res, colspec, '\n')

  cell_shadows_row <- cell_shadows(ht, 'row')
  cell_shadows_col <- cell_shadows(ht, 'col')
  for (myrow in 1:nrow(ht)) {
    for (mycol in 1:ncol(ht)) {
      if (cell_shadows_col[myrow, mycol]) next

      contents <- ht[myrow, mycol]
      if (! is.na(cell_color <- bgcolor(ht)[myrow, mycol])) {
        # \cellcolor[HTML]{AA0044}
        cell_color <- as.vector(col2rgb(cell_color))
        cell_color <- paste0(cell_color, collapse = ', ')
        cell_color <- paste0(c('\\cellcolor[RGB]{', cell_color, '}'), collapse = '')
        contents <- paste0(cell_color, ' ', contents)
      }
      if ((rs <- rowspan(ht)[myrow, mycol]) > 1) {
        # the tcb switch may only work with v recent multirow
        # ctb <- switch(valign(ht)[myrow, mycol],
        #       top    = 't',
        #       bottom = 'b',
        #       middle = ,
        #       'c')
        # * is width, could be more specific
        contents <- paste0('\\multirow{', rs,'}{*}{', contents,'}')
      }
      # must be this way round: multirow inside multicolumn
      if ((cs <- colspan(ht)[myrow, mycol]) > 1) {
        lcr <- switch(align(ht)[myrow, mycol],
              left   = 'l',
              right  = 'r',
              center = ,
              'c')
        contents <- paste0('\\multicolumn{', cs,'}{', lcr ,'}{', contents,'}')
      }
      if (! cell_shadows_row[myrow, mycol]) res <- paste0(res, contents)
      real_col <- sum(colspan(ht)[myrow, 1:mycol]) # but will this fail when we have multirows?
      if (real_col < ncol(ht)) res <- paste0(res, ' & ')
    }
    res <- paste0(res, ' \\tabularnewline\n')
  }

  res <- paste0(res, '\\end{tabularx}\n')
  res
}


#' @export
#'
#' @rdname to_latex
print_latex <- function (ht, ...) {
  cat(to_latex(ht, ...))
}
