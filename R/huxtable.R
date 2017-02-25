

#' @import tibble
#' @import knitr
#' @import rmarkdown
#' @import xtable
NULL




#' Create a huxtable
#'
#' @param ... Named list of values, as passed to \code{\link{data_frame}}.
#'
#' @return An object of class 'huxtable'.
#' @export
#'
#' @aliases hux
#' @examples
#' ht <- huxtable(column1 = 1:5, column2 = letters[1:5])
#'
#'
huxtable <- function (...) {
  ht <- tibble::data_frame(...)
  as_huxtable(ht)
}

#' @export
#' @rdname huxtable
hux <- huxtable

#' Convert an object to a huxtable
#'
#' @param dfr
#'
#' @return
#' @export
#'
#' @examples
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

  x <- tibble::as_tibble(x)
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
#' @param i
#' @param j
#' @param drop Not used.
#'
#' @return A huxtable.
#' @export
#'
#' @examples
`[.huxtable` <- function (x, i, j, drop = FALSE) {
  ss <- tibble::as_tibble(unclass(x))[i, j]
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
            rmarkdown::latex_dependency('tabularx'),
            rmarkdown::latex_dependency('siunitx'),
            rmarkdown::latex_dependency('xcolor', options = 'table'),
            rmarkdown::latex_dependency('multirow')
          )
    return(knitr::asis_output(res, meta = latex_deps))
  } else {
    return(knitr::asis_output(res))
  }
}

# also print_html, print_latex?

#' @export
print.huxtable <- function(ht, ...) {
  cat(to_screen(ht, ...))
}

#' @export
to_screen  <- function (ht, ...) UseMethod('to_screen')

#' @export
to_screen.huxtable <- function(ht, ...) {

}


#' @export
tbl_sum.huxtable <- function (x) {
  paste0('A huxtable: ', dim_desc(x))
}


as.tbl.huxtable <- function (x) {
  cl <- class(x)
  htcl <- which(cl == 'huxtable')
  class(x) <- class(x)[-(1:htcl)]
  x
}

clean_contents <- function(ht, row, col, type = c('latex', 'html'), ...) {
  mytype <- match.arg(type)
  contents <- ht[row, col]
  if (is.na(contents)) contents <- na_string(ht)[row, col]
  if (escape_contents(ht)[row, col]) {
    # xtable::sanitize.numbers would do very little and is buggy
    contents <-  xtable::sanitize(contents, type = mytype)
  }

  contents
}


