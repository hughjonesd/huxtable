
# functions for quick export to output formats------------------------------------------------------

#' @import assertthat
NULL


#' Quickly create a PDF, HTML, Word or Excel document showing matrices, data frames, et cetera.
#'
#' @param ... One or more huxtables or R objects with an `as_huxtable` method.
#' @param file File path for the output.
#' @param borders Border width for members of `...` that are not huxtables.
#' @param open Logical. Automatically open the resulting file?
#'
#' @return Invisible `NULL`.
#'
#' @details Objects in `...` will be converted to huxtables, with borders added.
#'
#' If \sQuote{file} is not specified, the default file path is "huxtable-output.xxx" in
#' the working directory. If the session is interactive, you'll be asked to confirm any
#' overwrite; if the session is not interactive, the command will fail.
#'
#' @examples
#' \dontrun{
#' m <- matrix(1:4, 2, 2)
#' dfr <- data.frame(a = 1:5, b = 1:5)
#' quick_pdf(m, dfr)
#' quick_html(m, dfr)
#' quick_docx(m, dfr)
#' quick_xlsx(m, dfr)
#' }
#' @name quick-output
NULL


#' @rdname quick-output
#' @export
quick_pdf <- function (..., file = confirm("huxtable-output.pdf"), borders = 0.4,
  open = interactive()) {
  assert_that(is.number(borders))
  force(file) # ensures confirm() is called before any other files are created.
  hts <- huxtableize(list(...), borders)
  # on my Mac, tempdir() gets a double slash in the path, which screws up texi2pdf.
  # You can't use normalizePath with a non-existent file, so the below doesn't work:
  # latex_file <- normalizePath(tempfile(fileext = ".tex"), mustWork = TRUE)
  clean_tmp_dir <- normalizePath(tempdir(), mustWork = TRUE)
  latex_file <- tempfile(tmpdir = clean_tmp_dir, fileext = ".tex")
  sink(latex_file)
  tryCatch({
    cat('\\documentclass{article}\n')
    report_latex_dependencies()
    cat('\n\\begin{document}')
    lapply(hts, function (ht) {
      cat('\n\n')
      print_latex(ht)
      cat('\n\n')
    })
    cat('\n\\end{document}')
  },
    error = identity,
    finally = {sink()}
  )

  tools::texi2pdf(latex_file, clean = TRUE) # outputs to current working directory
  pdf_file <- sub('\\.tex$', '.pdf', basename(latex_file))
  if (! file.exists(pdf_file)) stop('Could not find texi2pdf output file "', pdf_file, '"')
  if (! file.remove(latex_file)) warning('Could not remove intermediate TeX file "', latex_file, '"')
  # we overwrite existing files. If no explicit `file` argument was specified, confirm() has
  # already checked if this is OK, or has failed in non-interactive sessions:
  if (file.copy(pdf_file, file, overwrite = TRUE)) {
    file.remove(pdf_file)
  } else {
    stop('Could not copy pdf file to ', file, '. The pdf file remains at "', pdf_file, '"')
  }

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_html <- function (..., file = confirm("huxtable-output.html"), borders = 0.4,
  open = interactive()) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  sink(file)
  cat('<!DOCTYPE html><html><body>')
  tryCatch({
    lapply(hts, function (ht) {
      cat('<p>&nbsp;</p>')
      print_html(ht)
      cat('\n\n')
    })
    cat('</body></html>')
  },
    error = identity,
    finally = {sink()}
  )

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_docx <- function (..., file = confirm("huxtable-output.docx"), borders = 0.4,
  open = interactive()) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  my_doc <- officer::read_docx()
  for (ht in hts) {
    ft <- as_flextable(ht)
    my_doc <- flextable::body_add_flextable(my_doc, ft)
    my_doc <- officer::body_add_par(my_doc, " ")
  }
  print(my_doc, target = file)

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_xlsx <- function (..., file = confirm("huxtable-output.xlsx"), borders = 0.4,
  open = interactive()) {
  assert_that(is.number(borders))
  force(file)
  hts <- huxtableize(list(...), borders)
  wb <- openxlsx::createWorkbook()
  ix <- 0
  for (ht in hts) {
    ix <- ix + 1
    wb <- as_Workbook(ht, Workbook = wb, sheet = paste("sheet", ix))
  }
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

  if (open) auto_open(file)
  invisible(NULL)
}


huxtableize <- function (obj_list, borders) {
  lapply(obj_list, function (obj) {
    if (! inherits(obj, 'huxtable')) {
      obj <- as_huxtable(obj)
      obj <- set_all_borders(obj, borders)
    }
    obj
  })
}


confirm <- function (file) {
  if (! interactive()) stop('Please specify a `file` argument for non-interactive use of quick_xxx functions.')
  if (file.exists(file)) {
    answer <- readline(paste0('File "', file, '" already exists. Overwrite? [yN]'))
    if (! answer %in% c('y', 'Y')) stop('OK, stopping')
  }
  file
}

auto_open <- function (path) {
  sysname <- Sys.info()['sysname']
  switch(sysname,
    Darwin  = system2("open", path),
    Windows = system2("start", path),
    Linux   = system2("xdg-open", path),
    warning('Could not determine OS to open document automatically')
  )
}
