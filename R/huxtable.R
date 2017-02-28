
#' @import knitr
#' @import rmarkdown
#' @import xtable
NULL


#' Create a huxtable
#'
#' @param ... Named list of values, as for \code{\link{data.frame}}.
#'
#' @return An object of class 'huxtable'.
#' @export
#'
#' @aliases hux
#'
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
#'
#'
huxtable <- function (...) {
  ht <- data.frame(..., stringsAsFactors = FALSE)
  as_huxtable(ht)
}

#' @export
#' @rdname huxtable
hux <- huxtable

#' Convert an object to a huxtable
#'
#' @param x A suitable object. Methods exist for data frames, tables and matrices.
#'
#' @return An object of class \code{huxtable}.
#' @export
#'
#' @examples
#' dfr <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
#' as_huxtable(dfr)
#'
as_huxtable <- function(x, ...) UseMethod('as_huxtable')

#' @export
as_huxtable.default <- function (x, ...) {
  for (att in huxtable_cell_attrs) {
    attr(x, att) <- matrix(NA, nrow(x), ncol(x))
  }
  for (att in huxtable_col_attrs) {
    attr(x, att) <- rep(NA, ncol(x))
  }
  for (att in huxtable_row_attrs) {
    attr(x, att) <- rep(NA, nrow(x))
  }
  for (att in huxtable_table_attrs) {
    attr(x, att) <- NA
  }

  for (att in names(huxtable_default_attrs)) {
    attr(x, att)[] <- huxtable_default_attrs[[att]] # [[ indexing matters here
  }

  x <- as.data.frame(x)
  class(x) <- c('huxtable', class(x))
  x
}

#' @export
as_huxtable.table <- function(x, ...) {
  as_huxtable(as.matrix(x, ...))
}


#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j Columns to select.
#' @param drop Not used.
#'
#' @return A huxtable.
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = letters[1:3])
#' rowspan(ht)[2,1] <- 2
#' ht[1:2,]
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- as.data.frame(unclass(x), stringsAsFactors = FALSE)[i, j, drop]
  if (! missing(i) && is.character(i)) i <- which(rownames(ht) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(ht) %in% j)
  for (att in huxtable_cell_attrs) {
    attr(ss, att) <- attr(x, att)[i, j, drop = drop]
  }
  for (att in huxtable_col_attrs) {
    attr(ss, att) <- attr(x, att)[j]
  }
  for (att in huxtable_row_attrs) {
    attr(ss, att) <- attr(x, att)[i]
  }
  for (att in huxtable_table_attrs) {
    attr(ss, att) <- attr(x, att)
  }
  dcells <- display_cells(x)
  # check for dcells where row > nrow(ss) or col > ncol(ss) and display_row, display_col are within ss
  cut <- (dcells$row > nrow(ss) | dcells$col > ncol(ss)) & dcells$display_row <= nrow(ss) &
        dcells$display_col <= ncol(ss)
  if (any(cut)) warning('Some cells will be cut by subset')
  class(ss) <- class(x)
  for (r in which(cut)) {
    drow <- dcells$display_row[r]
    dcol <- dcells$display_col[r]
    colspan(ss)[drow, dcol] <- min(colspan(ss)[drow, dcol], ncol(ss) - dcol + 1)
    rowspan(ss)[drow, dcol] <- min(rowspan(ss)[drow, dcol], nrow(ss) - drow + 1)
  }
  ss
}



#' @export
knit_print.huxtable <- function (x, options, ...) {
  of <- rmarkdown::default_output_format(knitr::current_input())
  of <- of$name
  # not sure if 'print' is the right default here...
  call_name <- switch(of, pdf_document = 'to_latex', html_document = 'to_html', 'print')
  res <- do.call(call_name, list(ht=x))
  if (of == 'pdf_document') {
    latex_deps <- list(
            rmarkdown::latex_dependency('array'),
            rmarkdown::latex_dependency('graphicx'),
            rmarkdown::latex_dependency('siunitx'),
            rmarkdown::latex_dependency('xcolor', options = 'table'),
            rmarkdown::latex_dependency('multirow'),
            rmarkdown::latex_dependency('hhline')
          )
    tenv <- tabular_environment(x)
    if (tenv %in% c('tabularx', 'tabulary', 'longtable')) latex_deps <- c(latex_deps, list(rmarkdown::latex_dependency(tenv)))
    return(knitr::asis_output(res, meta = latex_deps))
  } else {
    return(knitr::asis_output(res))
  }
}


#' #' @export
#' print.huxtable <- function(ht, ...) {
#'   cat(to_screen(ht, ...))
#' }

#' @export
to_screen  <- function (ht, ...) UseMethod('to_screen')

#' @export
to_screen.huxtable <- function(ht, ...) {

}

clean_contents <- function(ht, row, col, type = c('latex', 'html'), ...) {
  mytype <- match.arg(type)
  # stopifnot(length(row) == 1 & length(col) == 1)
  contents <- ht[[row, col]] # just the data and just one element
  if (is.na(contents)) contents <- na_string(ht)[row, col]
  if (escape_contents(ht)[row, col]) {
    # xtable::sanitize.numbers would do very little and is buggy
    contents <-  xtable::sanitize(contents, type = mytype)
  }

  contents
}

# return matrix of cells displayed in a real 'cell position'
display_cells <- function(ht) {
  spans <- data.frame(row = rep(1:nrow(ht), ncol(ht)), col = rep(1:ncol(ht), each = nrow(ht)),
        rowspan = as.vector(rowspan(ht)), colspan = as.vector(colspan(ht)))
  spans$display_row <- spans$row
  spans$display_col <- spans$col
  spans$shadowed <- FALSE
  for (i in 1:nrow(spans)) {
    if (spans$rowspan[i] == 1 & spans$colspan[i] == 1) next
    dr <- spans$row[i]
    dc <- spans$col[i]
    spanned <- spans$row %in% dr:(dr + spans$rowspan[i] - 1) & spans$col %in% dc:(dc + spans$colspan[i] - 1)
    spans[spanned, c('display_row', 'display_col')] <- matrix(c(dr, dc), sum(spanned), 2, byrow = TRUE)
    shadowed <- spanned & (1:nrow(spans)) != i
    spans$shadowed[shadowed] <- TRUE
  }
  spans[, c('row', 'col', 'display_row', 'display_col', 'shadowed')]
}

# return matrix of whether cells are shadowed by a previous multirow/multicolumn cell
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



