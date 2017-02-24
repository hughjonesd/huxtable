

huxtable_cell_attrs <- c('align', 'valign', 'rowspan', 'colspan', 'bgcolor')
huxtable_col_attrs <- c('col_widths')
huxtable_row_attrs <- c()
huxtable_table_attrs <- c('width')
# list preserves different arg types:
huxtable_default_attrs <- list(rowspan = 1, colspan = 1, align = 'center', valign = 'middle',
    width = 1, col_widths = NA, bgcolor = NA)


make_getter_setters <- function(attr_name, attr_type = c('cell', 'row', 'col', 'table'), check_fun = NULL,
  check_values = NULL, extra_code = NULL) {
  attr_type <- match.arg(attr_type)
  funs <- list()

  funs[[attr_name]] <- eval(bquote(
    function(ht) UseMethod(.(attr_name))
  ))
  funs[[paste0(attr_name, '.huxtable')]] <- eval(bquote(
    function(ht) attr(ht, .(attr_name))
  ))

  setter <- paste0(attr_name, '<-')
  funs[[setter]] <- eval(bquote(
    function(ht, value) UseMethod(.(setter))
  ))

  check_fun <- if (! missing(check_fun)) bquote(stopifnot(.(check_fun)(value)))
  check_dims <- switch(attr_type,
    # ** got rid of these, because we want to be flexible in how values are specified, as with R assignment in general
    # cell  = quote(stopifnot(ncol(value) == ncol(ht) && nrow(value) == nrow(ht))),
    # row   = quote(stopifnot(length(value) == nrow(ht))),
    # col   = quote(stopifnot(length(value) == ncol(ht))),
    table = quote(stopifnot(length(value) == 1))
  )
  check_values <- if (! missing(check_values)) bquote(
    stopifnot(all(na.omit(value) %in% .(check_values)))
  )
  extra_code <- if (! missing(extra_code)) substitute(extra_code)
  funs[[paste0(setter, '.huxtable')]] <- eval(bquote(
    function(ht, value) {
      .(check_fun)
      .(check_dims)
      .(check_values)
      .(extra_code)
      attr(ht, attr_name)[] <- value
      ht
    }
  ))

  funs[[paste0('set_', attr_name)]] <- eval(bquote(
    function(ht, row, col, value) {
      .(as.name(attr_name))(ht)[row, col] <- value
      ht
    }
  ))

  lapply(names(funs), function (x) {
    assign(x, funs[[x]], envir = parent.frame(3)) # 3: 1 for function(x), 2 for lapply, 3 for the caller!
  })

  NULL
}

make_getter_setters('valign', 'cell', check_fun = is.character, check_values = c('top', 'middle', 'bottom'))


#' @template getset
#' @templatevar attr_name valign
#' @templatevar attr_desc Vertical Alignment
#' @templatevar value_param_desc A character vector or matrix which may be 'top', 'middle', 'bottom' or \code{NA}.

#' Get or set vertical alignment.
#'
#' @param ht A huxtable.
#' @param value Vertical alignment for specific cells.
#'
#' @return A matrix of alignment values. For `valign`, these may be 'top', 'middle',
#' 'bottom' or \code{NA}. For `align`, values may be 'left', 'center', 'middle' or \code{NA}.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' valign(ht)[2,2] <- 'top'
#' @name valign
NULL

#' @export
#' @name valign.huxtable
#' @rdname valign
NULL

#' @export
#' @name valign<-.huxtable
#' @rdname valign
NULL

#' @export
#' @name valign<-
#' @rdname valign
NULL


#' @export
#' @name set_valign
#' @rdname valign
NULL


#' @export
#' @rdname valign
align <- function (ht) UseMethod('align')

#' @export
align.huxtable <- function (ht) attr(ht, 'align')

#' @export
#' @rdname valign
`align<-` <- function (ht, value) UseMethod('align<-')

#' @export
`align<-.huxtable` <- function (ht, value = c('left', 'center', 'right', 'decimal')) {
  stopifnot(all(na.omit(value) %in% c('left', 'center', 'right', 'decimal')))
  attr(ht, 'align')[] <- value
  ht
}

#' Get or Set Table Width.
#'
#' @param ht A huxtable.
#' @param value Table width, a length-1 vector.
#'   If this is numeric it will be interpreted as a proportion of
#'   the containing element. If it is character, it will be interpreted by CSS or LaTeX.
#'
#' @return A single value.
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' width(ht) <- 0.8
#' @export
width <- function (ht) UseMethod('width')

#' @export
width.huxtable <- function (ht) attr(ht, 'width')

#' @export
#' @rdname width
`width<-` <- function (ht, value) UseMethod('width<-')

#' @export
`width<-.huxtable` <- function (ht, value) {
  stopifnot(length(value) == 1)
  attr(ht, 'width') <- value
  ht
}


#' Get or Set Column Widths.
#'
#' @param ht A huxtable.
#' @param value Column widths, a vector.
#'   If this is numeric it will be interpreted as a proportion of
#'   the table width. If it is character, it will be interpreted by CSS or LaTeX.
#'
#' @return A vector of column widths.
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' col_widths(ht) <- c(.3, .7)
#' @export
col_widths <- function(ht) UseMethod('col_widths')

#' @export
col_widths.huxtable <- function (ht) attr(ht, 'col_widths')

#' @export
#' @rdname col_widths
`col_widths<-` <- function (ht, value) UseMethod('col_widths<-')

#' @export
`col_widths<-.huxtable` <- function (ht, value) {
  stopifnot(length(value) == ncol(ht))
  attr(ht, 'col_widths') <- value
  ht
}

#' Get or Set Rowspan and Colspan.
#'
#' @param ht A huxtable.
#' @param value How many rows or columns should the cell span.
#'
#' @return A numeric matrix of row or column spans. \code{NA} means 1, the default.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' rowspan(ht)[1:2,1] <- 2
rowspan <- function (ht) UseMethod('rowspan')

#' @export
rowspan.huxtable <- function (ht) attr(ht, 'rowspan')

#' @export
#' @rdname rowspan
`rowspan<-` <- function (ht, value) UseMethod('rowspan<-')

#' @export
`rowspan<-.huxtable` <- function (ht, value) {
  stopifnot(is.numeric(value))
  if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop('rowspan would extend beyond bottom of table')
  attr(ht, 'rowspan')[] <- value

  # do we want to delete data that is shadowed by a row or colspan?
  # advantage: represents 'truth', i.e. this output is invisible
  # disadvantage: can't change rowspan repeatedly
  # disadvantage: can't stop people putting data back in!
  # maybe better not. Hard to manually maintain sanity. E.g. what if
  # cell 2,1 has rowspan 2; then someone makes cell 1,1 have rowspan 2 also?

  ht
}

#' @export
#' @rdname rowspan
colspan <- function (ht) UseMethod('colspan')

#' @export
colspan.huxtable <- function (ht) attr(ht, 'colspan')

#' @export
#' @rdname rowspan
`colspan<-` <- function (ht, value) UseMethod('colspan<-')

#' @export
`colspan<-.huxtable` <- function (ht, value) {
  stopifnot(is.numeric(value))
  if (any(na.omit( col(ht) + value - 1 > ncol(ht) ))) stop('rowspan would extend beyond bottom of table')
  attr(ht, 'colspan')[] <- value

  ht
}




#' Get or Set Cell Background Color
#'
#' @param ht A huxtable.
#' @param value Background color for the cell(s). NA is permissible
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' bgcolor(ht)[1:2,1] <- 'orange'
#' @export
bgcolor <- function (ht) UseMethod('bgcolor')

#' @export
bgcolor.huxtable <- function (ht) attr(ht, 'bgcolor')

#' @export
#' @rdname valign
`bgcolor<-` <- function (ht, value) UseMethod('bgcolor<-')

#' @export
`bgcolor<-.huxtable` <- function (ht, value) {
  attr(ht, 'bgcolor')[] <- value
  ht
}





# return matrix of whether cells are shadowed by rowspan from above or colspan from the left
cell_shadows <- function(ht, row_or_col) {
  row_or_col <- match.arg(row_or_col,  c('row', 'col'))
  spanfun  <- if (row_or_col == 'row') rowspan else colspan
  indexfun <- if (row_or_col == 'row') col else row

  spans <- spanfun(ht)
  # for each cell, what is the row/col number of its 'shadow'?
  coverage <- spans - 1 + indexfun(spans)
  # what is the farthest cell covered so far in each row/col?
  cum_coverage <- if (row_or_col == 'col') apply(coverage, 2, cummax) else
    t(apply(coverage, 1, cummax))
  # for each cell, is it within a shadow of something earlier?
  covered <- indexfun(spans) < cum_coverage
  covered <- if (row_or_col == 'col') cbind(FALSE,covered[, 1:(ncol(covered) - 1)]) else
    rbind(FALSE, covered[1:(nrow(covered) - 1), ])
  covered
}

# is a given cell 'shadowed'?
cell_shadowed <- function (ht, rn, cn, row_or_col = c('row', 'col')) {
  row_or_col <- match.arg(row_or_col)
  cell_shadows(ht, row_or_col)[rn, cn]
}

