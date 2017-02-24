

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

  alt_setter <- paste0('set_', attr_name)

  if (attr_type == 'cell') {
  funs[[alt_setter]] <- eval(bquote(
    function(ht, row, col, value) UseMethod(.(alt_setter))
  ))
  funs[[paste0(alt_setter, '.huxtable')]] <- eval(bquote(
    function(ht, row, col, value) {
      .(as.name(attr_name))(ht)[row, col] <- value
      ht
    }
  ))
  } else if (attr_type == 'row') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, row, value) UseMethod(.(alt_setter))
    ))
    funs[[paste0(alt_setter, '.huxtable')]] <- eval(bquote(
      function(ht, row, value) {
        .(as.name(attr_name))(ht)[row] <- value
        ht
      }
    ))
  } else if (attr_type == 'col') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, col, value) UseMethod(.(alt_setter))
    ))
    funs[[paste0(alt_setter, '.huxtable')]] <- eval(bquote(
      function(ht, col, value) {
        .(as.name(attr_name))(ht)[col] <- value
        ht
      }
    ))
  } else if (attr_type == 'table') {
    funs[[alt_setter]] <- eval(bquote(
      function(ht, value) UseMethod(.(alt_setter))
    ))
    funs[[paste0(alt_setter, '.huxtable')]] <- eval(bquote(
      function(ht, value) {
        .(as.name(attr_name))(ht) <- value
        ht
      }
    ))
  }

  lapply(names(funs), function (x) {
    assign(x, funs[[x]], envir = parent.frame(3)) # 3: 1 for function(x), 2 for lapply, 3 for the caller!
  })

  NULL
}


#' @template getset-cell
#' @templateVar attr_name valign
#' @templateVar attr_desc Vertical Alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'top', 'middle', 'bottom' or \code{NA}.
#' @export valign valign<- set_valign valign.huxtable valign<-.huxtable set_valign.huxtable
NULL
make_getter_setters('valign', 'cell', check_fun = is.character, check_values = c('top', 'middle', 'bottom'))


#' @template getset-table
#' @templateVar attr_name width
#' @templateVar attr_desc Table Width
#' @templateVar value_param_desc
#' A length-one vector. If numeric, it is treated as a proportion of the surrounding block width. If character, it must be a valid CSS or LaTeX width.
#' @export width width<- set_width width.huxtable width<-.huxtable set_width.huxtable
NULL
make_getter_setters('width', 'table')

#' @template getset-cell
#' @templateVar attr_name align
#' @templateVar attr_desc Alignment
#' @templateVar value_param_desc A character vector or matrix which may be 'left', 'center', 'right' or \code{NA}.
#' @export align align<- set_align align.huxtable align<-.huxtable set_align.huxtable
NULL
make_getter_setters('align', 'cell', check_fun = is.character, check_values = c('left', 'center', 'right'))

#' @template getset-col
#' @templateVar attr_name col_widths
#' @templateVar rowcol col
#' @templateVar attr_desc Column Widths
#' @templateVar value_param_desc A vector. If numeric, they are treated as proportions of the table width. If character, they must bevalid CSS or LaTeX lengths.
#' @export col_widths col_widths<- set_col_widths col_widths.huxtable col_widths<-.huxtable set_col_widths.huxtable
NULL
make_getter_setters('col_widths', 'col')

#' @template getset-cell
#' @templateVar attr_name rowspan
#' @templateVar attr_desc Row Span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @export rowspan rowspan<- set_rowspan rowspan.huxtable rowspan<-.huxtable set_rowspan.huxtable
NULL
make_getter_setters('rowspan', 'cell', check_fun = is.numeric, extra_code =
    if (any(na.omit( row(ht) + value - 1 > nrow(ht) ))) stop('rowspan would extend beyond bottom of table')
)

#' @template getset-cell
#' @templateVar attr_name colspan
#' @templateVar attr_desc Column Span
#' @templateVar value_param_desc An integer vector or matrix of integers.
#' @export colspan colspan<- set_colspan colspan.huxtable colspan<-.huxtable set_colspan.huxtable
NULL
make_getter_setters('colspan', 'cell', check_fun = is.numeric, extra_code =
    if (any(na.omit( col(ht) + value - 1 > ncol(ht) ))) stop('rowspan would extend beyond bottom of table')
)


#' @template getset-cell
#' @templateVar attr_name bgcolor
#' @templateVar attr_desc Cell Background Color
#' @templateVar value_param_desc A vector or matrix of R colors.
#' @export bgcolor bgcolor<- set_bgcolor bgcolor.huxtable bgcolor<-.huxtable set_bgcolor.huxtable
NULL
make_getter_setters('bgcolor', 'cell')




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

