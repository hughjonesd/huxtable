
#' Huxtable: Simply Create LaTeX and HTML Tables
#'
#' Huxtable is a package for creating HTML and LaTeX tables. It provides similar
#' functionality to xtable, with a simpler interface.
#'
#' @details
#'
#' To create a huxtable object, use \code{\link{huxtable}} or \code{\link{as_huxtable}}.
#'
#' For more information, see \href{https://hughjonesd.github.io/huxtable/}{the website}.
#' @name huxtable-package
NULL

#' @import knitr
#' @import htmltools
#' @import rmarkdown
#' @import xtable
#' @import stats
#' @import grDevices
#'
NULL


#' Create a Huxtable
#'
#' \code{huxtable}, or \code{hux}, creates a huxtable object.
#'
#' @param ... Named list of values, as for \code{\link{data.frame}}.
#' @param add_colnames If \code{TRUE}, add a first row of column names to the huxtable.
#' @param add_rownames If \code{TRUE}, add a first column of row names, named 'rownames', to the huxtable.
#'
#' @return An object of class \code{huxtable}.
#' @export
#'
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
huxtable <- function (..., add_colnames = FALSE, add_rownames = FALSE) {
  ht <- data.frame(..., stringsAsFactors = FALSE)
  ht <- as_huxtable(ht, add_colnames = add_colnames, add_rownames = add_rownames)

  ht
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
#' @rdname huxtable
as_huxtable.default <- function (x, add_colnames = FALSE, add_rownames = FALSE, ...) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
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

  # order matters here. We want original rownames, not anything else.
  if (add_rownames) x <- add_rownames(x, preserve_rownames = FALSE)
  if (add_colnames) x <- add_colnames(x)

  x <- set_attr_dimnames(x)
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
#' ht[1:2,]
#' ht[,1]
#' ht$a
#' \dontrun{
#' rowspan(ht)[2,1] <- 2
#' ht[1:2,] # generates a warning
#' }
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- as.data.frame(x)[i, j, drop]
  if (! missing(i) && is.character(i)) i <- which(rownames(x) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(x) %in% j)
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

  class(ss) <- class(x)
  colspan(ss) <- pmin(colspan(ss), 1 + ncol(ss) - col(ss))
  rowspan(ss) <- pmin(rowspan(ss), 1 + nrow(ss) - row(ss))

  ss <- set_attr_dimnames(ss)
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
#' ht
#' bold(ht)
#'
`[<-.huxtable` <- function(x, i, j, value) {
  if (! is_huxtable(value)) return(NextMethod())

  if (! missing(i) && is.character(i)) i <- which(rownames(x) %in% i)
  if (! missing(j) && is.character(j)) j <- which(colnames(x) %in% j)
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

  x <- NextMethod()
  x <- set_attr_dimnames(x)
  x
}


#' Add Rows/Columns
#'
#' @param ... Vectors, matrices, data frames or huxtables.
#' @param deparse.level Passed to \code{\link{cbind.data.frame}}.
#' @param copy_cell_props Cell properties to copy from neighbours (see below).
#'
#' @return A huxtable.
#'
#' @details
#' Table-level properties will be taken from the first argument which is a huxtable. So will
#' row heights (for cbind) and column widths (for rbind).
#'
#' If some of the inputs are not huxtables, and \code{copy_cell_props} is a character vector,
#' then for rbind, cell properties and row heights will be copied to non-huxtables. For cbind,
#' cell properties and column widths will be copied. Objects on the left or above get priority
#' over those on the right or below.
#'
#' If \code{copy_cell_props} is \code{TRUE}, the default
#' set of cell properties (everything but \code{colspan} and \code{rowspan}) will be copied.
#'
#' If \code{copy_cell_props} is \code{FALSE}, cells from non-huxtable objects will get the
#' default properties.
#'
#' @examples
#' ht1 <- hux(a = 1:3, b = 4:6)
#' ht2 <- hux(d = letters[1:3], e = letters[4:6])
#' bold(ht1)[1,] <- TRUE
#' bold(ht2) <- TRUE
#' vec <- LETTERS[1:3]
#'
#' ht_out <- cbind(ht1, vec, ht2)
#' ht_out
#' bold(ht_out)
#' bold(cbind(ht1, vec, ht2, copy_cell_props = FALSE))
#'
#' @export
cbind.huxtable <- function(..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  bind_hux(..., type = 'cbind', copy_cell_props = copy_cell_props)
}


#' @export
#' @rdname cbind.huxtable
rbind.huxtable <- function(..., deparse.level = 1, copy_cell_props = TRUE) {
  force(copy_cell_props)
  bind_hux(..., type = 'rbind', copy_cell_props = copy_cell_props)
}


bind_hux <- function(..., type, copy_cell_props) {
  default_copy_attrs <- setdiff(huxtable_cell_attrs, c('colspan', 'rowspan'))
  if (isTRUE(copy_cell_props)) copy_cell_props <- default_copy_attrs
  objs <- list(...)
  arg_names <- names(sapply(substitute(list(...))[-1], deparse))

  objs <- lapply(seq_along(objs), function(idx) {
    x <- objs[[idx]]
    if (is.vector(x) || is.factor(x)) {
      x <- as.matrix(x)
      if (! is.null(arg_names) && nzchar(arg_names[idx])) colnames(x) <- arg_names[idx]
      if (type == 'rbind') x <- t(x)
    }
    attr(x, 'from_real_hux') <- is_hux(x)
    x
  })

  f <- function(ht, x) bind2_hux(ht, x, type, copy_cell_props = copy_cell_props)
  res <- Reduce(f, objs)

  daddy <- Find(is_hux, objs)
  first_attrs <- switch(type, 'cbind' = huxtable_row_attrs, 'rbind' = huxtable_col_attrs)
  for (att in c(first_attrs, huxtable_table_attrs)) attr(res, att) <- attr(daddy, att)

  attr(res, 'from_real_hux') <- NULL
  res
}


bind2_hux <- function(ht, x, type, copy_cell_props) {
  ht_real_hux <- attr(ht, 'from_real_hux')
  x_real_hux  <- attr(x, 'from_real_hux')

  ht <- as_hux(ht) # resets attributes
  x  <- as_hux(x)
  ccp <- intersect(copy_cell_props, huxtable_cell_attrs)

  if (is.character(ccp)) {
    if (! x_real_hux) {
      for (att in ccp) {
        attr(x, att)[] <- if (type == 'cbind') attr(ht, att)[, ncol(ht)] else
          matrix(attr(ht, att)[nrow(ht),], nrow(x), ncol(x), byrow = TRUE)
      }
    }
    if (! ht_real_hux && x_real_hux) {
      for (att in ccp) {
        attr(ht, att)[] <- if (type == 'cbind') attr(x, att)[,1] else
          matrix(attr(x, att)[1,], nrow(ht), ncol(ht), byrow = TRUE)
      }
    }
  }

  bind_df <- switch(type, 'cbind' = cbind.data.frame, 'rbind' = function(x,y){
    rbind.data.frame(x, setNames(y, names(x)), stringsAsFactors = FALSE)
  })
  bind_cells <- switch(type, 'cbind' = cbind, 'rbind' = rbind)

  res <- as_hux(bind_df(ht, x))
  for (att in huxtable_cell_attrs) {
    attr(res, att) <- bind_cells(attr(ht, att), attr(x, att))
  }
  join_attrs <- switch(type, 'cbind' = huxtable_col_attrs, 'rbind' = huxtable_row_attrs)
  for (att in join_attrs) {
    attr(res, att) <- c(attr(ht, att), attr(x, att))
  }

  attr(res, 'from_real_hux') <- x_real_hux || ht_real_hux
  res
}

#' @export
`dimnames<-.huxtable` <- function(x, value) {
  x <- NextMethod()
  x <- set_attr_dimnames(x)
  x
}

#' Transpose a Huxtable
#'
#' @param x A huxtable.
#'
#' @return The transposed object.
#'
#' @details
#' Row and column spans of \code{x} will be swapped, as will column widths and row heights,
#' table width and height, and cell borders (bottom becomes right, etc.).
#' Other properties - in particular, alignment, vertical alignment and rotation - will be
#' preserved.
#' @examples
#' ht <- huxtable(a = 1:3, b = 1:3)
#' bottom_border(ht)[3,] <- 1
#' t(ht)
#'
#' @export
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
  attr(res, 'right_border')  <- t(attr(x, 'bottom_border'))
  attr(res, 'left_border')   <- t(attr(x, 'top_border'))
  attr(res, 'top_border')    <- t(attr(x, 'left_border'))
  row_height(res) <- col_width(x)
  col_width(res)  <- row_height(x)
  rownames(res)   <- colnames(x)
  colnames(res)   <- rownames(x)
  for (att in huxtable_table_attrs) {
    attr(res, att) <- attr(x, att)
  }
  res
}




#' Add Column or Row Names
#'
#' Add a first row of column names, or a first column of row names, to the huxtable.
#'
#' Note that \code{add_colnames} will change the mode of all columns to character.

#' \code{add_colnames} preserves column names. \code{add_rownames} only preserves them if asked to.
#'
#' @param ht A huxtable.
#' @param colname Column name for the new column of row names.
#' @param rowname Optional row name for the new row of column names.
#' @param preserve_rownames Preserve existing row names.
#' @param ... Arguments passed to methods.
#'
#' @return The modified object.
#'
#' @examples
#' ht <- huxtable(a = 1:5, b = 1:5)
#' add_rownames(ht)
#' add_colnames(ht)
#' add_rownames(add_colnames(ht)) # Out by 1
#' add_colnames(add_rownames(ht)) # Better
#' add_colnames(add_rownames(ht, '')) # Alternatively
#'
#' @export
add_colnames <- function (ht, ...) UseMethod('add_colnames')

#' @export
#' @rdname add_colnames
add_colnames.huxtable <- function (ht, rowname = NULL, ...) {
  cn <- colnames(ht)
  ht <- rbind(cn, ht)
  colnames(ht) <- cn
  if (! missing(rowname)) rownames(ht) <- c(rowname, rownames(ht)[1:(nrow(ht) - 1)])
  ht
}

#' @export
#' @rdname add_colnames
add_rownames <- function (ht, ...) UseMethod('add_rownames')

#' @export
#' @rdname add_colnames
add_rownames.huxtable <- function (ht, colname = 'rownames', preserve_rownames = TRUE, ...) {
  ht <- cbind(rownames(ht), ht)
  colnames(ht)[1] <- colname
  if (! preserve_rownames) rownames(ht) <- NULL
  ht
}

set_attr_dimnames <- function(ht) {
  for (att in huxtable_cell_attrs) {
    dimnames(attr(ht, att)) <- dimnames(ht)
  }
  for (att in huxtable_col_attrs) {
    names(attr(ht, att)) <- dimnames(ht)[[2]]
  }
  for (att in huxtable_row_attrs) {
    names(attr(ht, att)) <- dimnames(ht)[[1]]
  }

  ht
}

#' @export
knit_print.huxtable <- function (x, options, ...) {
  of <- rmarkdown::default_output_format(knitr::current_input())
  of <- of$name
  # not sure if 'print' is the right default here...
  call_name <- switch(of, pdf_document = 'to_latex', html_document = 'to_html', 'to_screen')
  res <- do.call(call_name, list(ht=x))
  if (of == 'pdf_document') {
    latex_deps <- report_latex_dependencies(quiet = TRUE)
    tenv <- tabular_environment(x)
    if (tenv %in% c('tabulary', 'longtable')) latex_deps <- c(latex_deps, list(rmarkdown::latex_dependency(tenv)))
    return(knitr::asis_output(res, meta = latex_deps))
  } else {
    return(knitr::asis_output(res))
  }
}


#' Huxtable Logo
#'
#' @return The huxtable logo
#' @export
#'
#' @examples
#' print_screen(hux_logo())
#'
hux_logo <- function() {
  logo <- hux(c('h', NA), c('u', 'table'), c('x', NA))
  rowspan(logo)[1, 1] <- 2
  colspan(logo)[2, 2] <- 2
  logo <- set_all_borders(logo,,,1)
  font_size(logo) <- 20
  font_size(logo)[1, 2:3] <- 24
  font_size(logo)[1, 1] <- 54
  background_color(logo)[1, 1] <- '#e83abc'
  background_color(logo)[1, 3] <- 'black'
  text_color(logo)[1, 3] <- 'white'
  width(logo) <- '140pt'
  font(logo) <- 'Palatino,Palatino Linotype,"Palatino LT STD",Book Antiqua,Georgia,serif'
  top_padding(logo)[1, 1] <- 5
  bottom_padding(logo)[1, 1] <- 5
  left_padding(logo)[1, 1] <- 10
  logo
}






