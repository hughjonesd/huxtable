
#' @import knitr
#' @import rmarkdown
#' @import xtable
NULL


#' Create a Huxtable
#'
#' \code{huxtable}, or \code{hux}, creates a huxtable object.
#'
#' @param ... Named list of values, as for \code{\link{data.frame}}.
#' @param col_names If \code{TRUE}, a first row of column names will be added to the huxtable.
#' @param row_names If \code{TRUE}, a first column of row names, named "rownames", will be added to the huxtable.
#'
#' @return An object of class \code{huxtable}.
#' @export
#'
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
huxtable <- function (..., col_names = FALSE, row_names = FALSE) {
  ht <- data.frame(..., stringsAsFactors = FALSE)

  # order matters here. We want original rownames, not anything else.
  cn <- colnames(ht)
  if (row_names) cn <- c('', cn)
  if (row_names) ht <- cbind(rownames = rownames(ht), ht, stringsAsFactors = FALSE)
  if (col_names) ht <- rbind(cn, ht, stringsAsFactors = FALSE)

  as_huxtable(ht)
}

#' @export
#' @rdname huxtable
hux <- huxtable


#' @param x An object to convert to a huxtable.
#'
#' @export
#' @details
#' \code{as_huxtable} and \code{as_hux} convert an object to a huxtable.
#' Conversion methods exist for data frames, tables, ftables, matrices and (most) vectors.
#' @examples
#' dfr <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
#' as_huxtable(dfr)
#'
#' @rdname huxtable
as_huxtable <- function(x, ...) UseMethod('as_huxtable')

#' @export
#' @rdname huxtable
as_hux <- as_huxtable

#' @export
as_huxtable.default <- function (x, ...) {
  x <- as.data.frame(x)
  for (att in setdiff(huxtable_cell_attrs, 'number_format')) {
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
  attr(x, 'number_format') <- matrix(list(NA), nrow(x), ncol(x))
  for (att in names(huxtable_default_attrs)) {
    attr(x, att)[] <- huxtable_default_attrs[[att]] # [[ indexing matters here
  }

  class(x) <- c('huxtable', class(x))
  x
}

#' @export
as_huxtable.huxtable <- function(x, ...) x

#' @export
as_huxtable.table <- function(x, ...) as_huxtable(unclass(x), ...)

#' @export
as_huxtable.ftable <- function(x, ...) as_huxtable(format(x, quote = FALSE), ...)


#' @export
as_huxtable.numeric <- function (x, ...) {
  # use default otherwise matrix has class e.g. c('matrix', 'numeric') so we recurse
  as_huxtable.default(as.matrix(x, ...))
}

#' @export
as_huxtable.character <- as_huxtable.numeric

#' @export
as_huxtable.logical   <- as_huxtable.numeric

#' @export
as_huxtable.complex   <- as_huxtable.numeric

#' @export
#' @rdname huxtable
is_huxtable <- function(x) inherits(x, 'huxtable')

#' @export
#' @rdname huxtable
is_hux <- is_huxtable

#' Subset a huxtable
#'
#' @param x A huxtable.
#' @param i Rows to select.
#' @param j Columns to select.
#' @param drop Not used.
#'
#' @return A huxtable.
#' @export
#' @rdname extract-methods
#' @details
#' \code{[} always returns a new huxtable object, while \code{$} and \code{[[} simply
#' return a vector of data.
#' For the replacement function \code{[<-}, if \code{value} is a huxtable, then its cell properties will be
#' copied into \code{x}. In addition, if \code{nrow(value) == nrow(x)}, then column properties
#' will be copied into \code{x} as appropriate, and if  \code{ncol(value) == ncol(x)}, then
#' row properties will be copied.
#' Replacement functions \code{$<-} and \code{[[<-} simply change the data without affecting other properties.
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



#' @param value A matrix, data frame, huxtable or similar object.
#'
#' @rdname extract-methods
#' @export
#'
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' ht2 <- huxtable(10:11, 12:13)
#' bold(ht2) <- TRUE
#' ht[2:3,] <- ht2
#' bold(ht)
#'
`[<-.huxtable` <- function(x, i, j, value) {
  if (! is_huxtable(value)) return(NextMethod())

  if (! missing(i) && is.character(i)) i <- which(rownames(ht) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(ht) %in% j)
  for (att in huxtable_cell_attrs) {
    attr(x, att)[i, j] <- attr(value, att)
  }
  if (nrow(value) == nrow(x)) {
    for (att in huxtable_col_attrs) {
      attr(x, att)[j] <- attr(value, att)
    }
  }
  if (ncol(value) == ncol(x)) {
    for (att in huxtable_row_attrs) {
      attr(x, att)[i] <- attr(value, att)
    }
  }

  NextMethod() # returns the object to be reassigned to x
}


#' Add Rows/Columns
#'
#' @param ... Vectors, matrices, data frames or huxtables.
#' @param deparse.level Passed to \code{\link{cbind.data.frame}}
#'
#' @return A huxtable.
#'
#' @details
#' Table-level properties will be taken from the first argument. Row-level
#' properties will be taken from the first argument to \code{cbind}, and
#' column-level properties from the first argument to \code{rbind}.
#'
#' If the first argument to \code{cbind} is not a \code{huxtable}, then
#' \code{cbind.huxtable} will not be called. To avoid this, do e.g.
#' \code{cbind(hux(1:5), ht)}.
#' @examples
#' ht1 <- hux(a = 1:3, b = 1:3)
#' bold(ht1) <- TRUE
#' ht2 <- hux(d = letters[1:3])
#' vec <- LETTERS[1:3]
#' ht <- cbind(ht1, ht2, vec)
#' ht
#' bold(ht)
#'
#' wrong <- cbind(vec, ht)
#' bold(wrong) # uh-oh
#' right <- cbind(as_hux(vec), ht)
#' bold(right)
#' @export
cbind.huxtable <- function(..., deparse.level = 1) {
  Reduce(cbind2_hux, list(...))
}

#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function(..., deparse.level = 1) {
  Reduce(rbind2_hux, list(...))
}

cbind2_hux <- function(ht, x) bind2_hux(ht, x, 'cbind')
rbind2_hux <- function(ht, x) bind2_hux(ht, x, 'rbind')

bind2_hux <- function(ht, x, type) {
  if (type=='rbind') {
    if (is.vector(x) || is.factor(x)) x <- t(x)
    if (is.vector(ht) || is.factor(ht)) ht <- t(ht)
  }
  ht <- as_hux(ht)
  x <- as_hux(x)
  bind_df <- switch(type, 'cbind' = cbind.data.frame, 'rbind' = function(x,y){
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })
  bind_cells <- switch(type, 'cbind' = cbind, 'rbind' = rbind)

  res <- as_hux(bind_df(ht, x))
  for (att in huxtable_cell_attrs) {
    attr(res, att) <- bind_cells(attr(ht, att), attr(x, att))
  }
  join_attrs <- switch(type, 'cbind' = huxtable_col_attrs, 'rbind' = huxtable_row_attrs)
  first_attrs <- switch(type, 'cbind' = huxtable_row_attrs, 'rbind' = huxtable_col_attrs)
  for (att in join_attrs) {
    attr(res, att) <- c(attr(ht, att), attr(x, att))
  }
  for (att in first_attrs) {
    attr(res, att) <- attr(ht, att)
  }
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(ht, att)
  }
  res
}

#' Transpose a Huxtable
#'
#' @param x A huxtable.
#'
#' @return The transposed object.
#' @export
#'
#' @details
#' Row and column spans of \code{x} will be swapped, as will column widths and row heights,
#' table width and height, and cell borders (bottom becomes right, etc.).
#' Other properties - in particular, alignment, vertical alignment and rotation - will be
#' preserved.
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' bottom_border(ht)[3,] <- 1
#' ht_trans <- t(ht)
#' ht_trans
t.huxtable <- function (x) {
  res <- as_hux(NextMethod())
  for (att in setdiff(huxtable_cell_attrs, c('colspan', 'rowspan', 'height', 'width',
        'bottom_border', 'left_border', 'top_border', 'right_border'))) {
    attr(res, att) <- t(attr(x, att))
  }
  attr(res, 'colspan') <- t(attr(x, 'rowspan'))
  attr(res, 'rowspan') <- t(attr(x, 'colspan'))
  attr(res, 'width')   <- attr(x, 'height')
  attr(res, 'height')  <- attr(x, 'width')
  attr(res, 'bottom_border') <- t(attr(x, 'right_border'))
  attr(res, 'right_border') <- t(attr(x, 'bottom_border'))
  attr(res, 'left_border') <- t(attr(x, 'top_border'))
  attr(res, 'top_border') <- t(attr(x, 'left_border'))
  row_height(res)      <- col_width(x)
  col_width(res)       <- row_height(x)
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(x, att)
  }
  res
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
            rmarkdown::latex_dependency('hhline'),
            rmarkdown::latex_dependency('calc')
          )
    tenv <- tabular_environment(x)
    if (tenv %in% c('tabularx', 'tabulary', 'longtable')) latex_deps <- c(latex_deps, list(rmarkdown::latex_dependency(tenv)))
    return(knitr::asis_output(res, meta = latex_deps))
  } else {
    return(knitr::asis_output(res))
  }
}

#' @export
to_md <- function(ht, ...) UseMethod('to_md')

#' Create Markdown Representing a Huxtable
#'
#' @param ht
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
to_md.huxtable <- function(ht, ...) {

}


clean_contents <- function(ht, row, col, type = c('latex', 'html', 'screen'), ...) {
  mytype <- match.arg(type)
  # stopifnot(length(row) == 1 & length(col) == 1)
  contents <- ht[[row, col]] # just the data and just one element.
  # But we might want to allow more than one element; if so just use `[.data.frame`
  if (! is.na(cnum <- suppressWarnings(as.numeric(contents)))) {
    nf <- number_format(ht)[[row, col]] # a list element
    if (is.function(nf)) contents <- nf(cnum)
    if (is.character(nf)) contents <- sprintf(nf, cnum)
    if (is.numeric(nf)) contents <- formatC(round(cnum, nf), format = 'f', digits = nf)
  }

  if (is.na(contents)) contents <- na_string(ht)[row, col]
  if (escape_contents(ht)[row, col] && type != 'screen') {
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




