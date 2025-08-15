# functions for quick export to output formats------------------------------------------------------

#' @import assertthat
NULL


#' Quickly print objects to a PDF, TeX, Typst, HTML, Microsoft Office or RTF document,
#' or PNG or SVG images
#'
#' These functions use huxtable to print objects to an output document. They are useful
#' as one-liners for data reporting.
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
#' If \sQuote{file} is not specified, the command will fail in non-interactive sessions. In
#' interactive sessions, the default file path is "huxtable-output.xxx" in the working directory;
#' if this already exists, you will be asked to confirm manually before proceeding.
#'
#' To create docx and pptx files `flextable` and `officer` must be installed, while xlsx
#' needs `openxlsx`. `quick_typst_pdf()`, `quick_typst_png()`, and `quick_typst_svg()` require the `typst`
#' command line tool.
#'
#' @examples
#' \dontrun{
#' m <- matrix(1:4, 2, 2)
#'
#' quick_pdf(m, jams)
#' quick_latex(m, jams)
#' quick_typst(m, jams)
#' quick_typst_pdf(m, jams)
#' quick_typst_png(m, jams)
#' quick_typst_svg(m, jams)
#' quick_html(m, jams)
#' quick_docx(m, jams)
#' quick_xlsx(m, jams)
#' quick_pptx(m, jams)
#' quick_rtf(m, jams)
#' }
#' @name quick-output
NULL


#' @rdname quick-output
#' @export
quick_latex <- function(
    ..., file = confirm("huxtable-output.tex"), borders = 0.4,
    open = interactive()) {
  assert_that(is.number(borders), is.flag(open))
  force(file)

  hts <- huxtableize(list(...), borders)
  do_write_latex_file(hts, file, width = NULL, height = NULL)

  if (open) auto_open(file)

  invisible(NULL)
}


#' @rdname quick-output
#' @param width String passed to the LaTeX `geometry` package's `paperwidth` option or Typst's `page`
#'   `width` option. Use `NULL` for the default width.
#' @param height String passed to the LaTeX `geometry` package's `paperheight` option or Typst's
#'   `page` `height` option. Use `NULL` for the default height.
#' @export
quick_pdf <- function(
    ..., file = confirm("huxtable-output.pdf"), borders = 0.4,
    open = interactive(), width = NULL, height = NULL) {
  assert_that(is.number(borders), is.string(width) || is.null(width), is.string(height) || is.null(height))
  assert_that(is.flag(open))
  force(file) # ensures confirm() is called before any other files are created.
  hts <- huxtableize(list(...), borders)

  # on my Mac, tempdir() gets a double slash in the path, which screws up texi2pdf.
  # You can"t use normalizePath with a non-existent file, so the below doesn"t work:
  # latex_file <- normalizePath(tempfile(fileext = ".tex"), mustWork = TRUE)
  clean_tmp_dir <- normalizePath(tempdir(), mustWork = TRUE)
  latex_file <- tempfile(tmpdir = clean_tmp_dir, fileext = ".tex")
  do_write_latex_file(hts, latex_file, width, height)

  if (requireNamespace("tinytex", quietly = TRUE)) {
    engine <- if (getOption("huxtable.latex_use_fontspec", FALSE)) "xelatex" else "pdflatex"
    tinytex::latexmk(latex_file, pdf_file = file, engine = engine)
    output_file <- file
  } else {
    if (getOption("huxtable.latex_use_fontspec", FALSE)) {
      old_LATEX <- Sys.getenv("LATEX")
      Sys.setenv(LATEX = "xelatex")
      tools::texi2dvi(latex_file, clean = TRUE) # outputs to current working directory
      Sys.setenv(LATEX = old_LATEX)
    } else {
      tools::texi2pdf(latex_file, clean = TRUE) # outputs to current working directory
    }
    output_file <- sub("\\.tex$", ".pdf", basename(latex_file))
  }
  if (!file.exists(output_file)) stop("Could not find pdf output file '", output_file, "'")
  if (!file.remove(latex_file)) warning("Could not remove intermediate TeX file '", latex_file, "'")

  if (output_file != file) {
    if (file.copy(output_file, file, overwrite = TRUE)) {
      file.remove(output_file)
    } else {
      stop("Could not copy pdf file to ", file, ". The pdf file remains at '", output_file, "'")
    }
  }

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_typst <- function(
    ..., file = confirm("huxtable-output.typ"), borders = 0.4,
    open = interactive()) {
  assert_that(is.number(borders), is.flag(open))
  force(file)

  hts <- huxtableize(list(...), borders)
  do_write_typst_file(hts, file, width = NULL, height = NULL)

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
#' @details
#'
#' `quick_typst_pdf()` with e.g. `file = "foo.pdf"` will overwrite and delete
#' the file `foo.typ`.
quick_typst_pdf <- function(
    ..., file = confirm("huxtable-output.pdf"), borders = 0.4,
    open = interactive(), width = NULL, height = NULL) {
  assert_that(is.number(borders), is.string(width) || is.null(width), is.string(height) || is.null(height))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)

  typst_file <- sub("\\.pdf$", ".typ", file)
  do_write_typst_file(hts, typst_file, width, height)

  if (Sys.which("typst") != "") {
    res <- system2("typst", c("compile", typst_file, file))
  } else if (Sys.which("quarto") != "") {
    res <- system2("quarto", c("typst", "compile", typst_file, file))
  } else {
    stop("Could not find typst or quarto CLI. Please install typst or quarto to create PDFs.")
  }

  if (res != 0) {
    stop("Typst compilation failed")
  }
  if (!file.remove(typst_file)) warning("Could not remove intermediate Typst file '", typst_file, "'")

  if (!file.exists(file)) stop("Could not find pdf output file '", file, "'")
  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
#' @param ppi Pixels per inch for PNG output.
#' @details
#' `quick_typst_png()` and `quick_typst_svg()` create one image per huxtable. If there is more than
#' one object in `...`, images will have a numeric suffix like `"-1", "-2"` etc.
#' Existing files with the same `file` prefix will be overwritten after
#' confirmation in interactive sessions.
quick_typst_png <- function(
    ..., file = confirm_prefix("huxtable-output"), borders = 0.4,
    open = interactive(), width = NULL, height = NULL, ppi = NULL) {
  assert_that(
    is.number(borders), is.flag(open),
    is.string(width) || is.null(width),
    is.string(height) || is.null(height),
    is.number(ppi) || is.null(ppi)
  )
  force(file)
  hts <- huxtableize(list(...), borders)
  extra_args <- if (!is.null(ppi)) c("--ppi", as.character(ppi)) else character()
  do_quick_typst_images(hts,
    file       = file,
    open       = open,
    width      = width,
    height     = height,
    format     = "png",
    extra_args = extra_args
  )
}

#' @rdname quick-output
#' @export
quick_typst_svg <- function(
    ..., file = confirm_prefix("huxtable-output"), borders = 0.4,
    open = interactive(), width = NULL, height = NULL) {
  assert_that(
    is.number(borders), is.flag(open),
    is.string(width) || is.null(width),
    is.string(height) || is.null(height)
  )
  force(file)
  hts <- huxtableize(list(...), borders)
  do_quick_typst_images(hts,
    file   = file,
    open   = open,
    width  = width,
    height = height,
    format = "svg"
  )
}


#' @rdname quick-output
#' @export
quick_html <- function(
    ..., file = confirm("huxtable-output.html"), borders = 0.4,
    open = interactive()) {
  assert_that(is.number(borders))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)

  loc <- Sys.getlocale("LC_COLLATE")
  loc <- strsplit(loc, ".", fixed = TRUE)[[1]]
  loc[1] <- gsub("_", "-", loc[1], fixed = TRUE)
  loc <- ifelse(is.na(loc), "", loc)
  sink(file)
  cat("<!DOCTYPE html>",
    sprintf("<html lang=\"%s\">", loc[1]),
    sprintf(
      "<head><meta charset=\"%s\"><title>%s</title></head>",
      loc[2], basename(file)
    ),
    "<body>\n",
    sep = "\n"
  )
  tryCatch(
    {
      lapply(hts, function(ht) {
        cat("<p>&nbsp;</p>")
        print_html(ht)
        cat("\n\n")
      })
      cat("</body></html>")
    },
    error = identity,
    finally = {
      sink()
    }
  )

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_docx <- function(
    ..., file = confirm("huxtable-output.docx"), borders = 0.4,
    open = interactive()) {
  assert_that(is.number(borders))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)
  assert_package("quick_docx", "officer")
  assert_package("quick_docx", "flextable")

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
quick_pptx <- function(
    ..., file = confirm("huxtable-output.pptx"), borders = 0.4,
    open = interactive()) {
  assert_that(is.number(borders))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)
  assert_package("quick_pptx", "officer")
  assert_package("quick_pptx", "flextable") # needed for as_flextable() below


  my_pptx <- officer::read_pptx()
  for (ht in hts) {
    ft <- as_flextable(ht)
    my_pptx <- officer::add_slide(my_pptx, layout = "Title and Content", master = "Office Theme")
    my_pptx <- officer::ph_with(my_pptx, ft, location = officer::ph_location_type("body"))
  }
  print(my_pptx, target = file)

  if (open) auto_open(file)
  invisible(NULL)
}


#' @rdname quick-output
#' @export
quick_xlsx <- function(..., file = confirm("huxtable-output.xlsx"), borders = 0.4,
                       open = interactive()) {
  assert_that(is.number(borders))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)
  assert_package("quick_xlsx", "openxlsx")

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


#' @rdname quick-output
#' @export
quick_rtf <- function(..., file = confirm("huxtable-output.rtf"), borders = 0.4,
                      open = interactive()) {
  assert_that(is.number(borders))
  assert_that(is.flag(open))
  force(file)
  hts <- huxtableize(list(...), borders)

  fc_tbls <- do.call(rtf_fc_tables, hts)

  sink(file)
  tryCatch(
    {
      cat("{\\rtf1\\ansi\\deff0\n")
      print(fc_tbls)
      cat("\n\n\n")
      lapply(hts, print_rtf)
      cat("\n\n\n}")
    },
    error = identity,
    finally = {
      sink()
    }
  )

  if (open) auto_open(file)
  invisible(NULL)
}


huxtableize <- function(obj_list, borders) {
  lapply(obj_list, function(obj) {
    if (!inherits(obj, "huxtable")) {
      obj <- as_huxtable(obj)
      obj <- set_all_borders(obj, borders)
    }
    obj
  })
}


do_write_latex_file <- function(hts, file, width, height) {
  sink(file)
  tryCatch(
    {
      cat("\\documentclass{article}\n")
      report_latex_dependencies()
      if (!is.null(width) || !is.null(height)) {
        dim_string <- character(2)
        dim_string[1] <- if (is.null(width)) "" else sprintf("paperwidth=%s", width)
        dim_string[2] <- if (is.null(height)) "" else sprintf("paperheight=%s", height)
        dim_string <- paste(dim_string, collapse = ",")
        cat(sprintf("\\usepackage[%s]{geometry}\n", dim_string))
      }
      cat("\n\\pagenumbering{gobble}\n")
      cat("\n\\begin{document}")
      lapply(hts, function(ht) {
        cat("\n\n")
        print_latex(ht)
        cat("\n\n")
      })
      cat("\n\\end{document}")
    },
    error = identity,
    finally = {
      sink()
    }
  )
}


#' Write a Typst document containing huxtables
#'
#' @param hts List of huxtables.
#' @param file Output file path.
#' @param width Page width string or `NULL`.
#' @param height Page height string or `NULL`.
#' @param page_break Logical. Insert `#pagebreak()` between tables?
#'
#' @noRd
do_write_typst_file <- function(hts, file, width, height, page_break = FALSE) {
  sink(file)
  tryCatch(
    {
      if (!is.null(width) || !is.null(height)) {
        dim_parts <- c()
        if (!is.null(width)) dim_parts <- c(dim_parts, sprintf("width: %s", width))
        if (!is.null(height)) dim_parts <- c(dim_parts, sprintf("height: %s", height))
        cat("#set page(", paste(dim_parts, collapse = ", "), ")\n\n", sep = "")
      }
      cat("#show link: underline\n\n")
      lapply(seq_along(hts), function(i) {
        cat(to_typst(hts[[i]]))
        if (page_break && i < length(hts)) {
          cat("\n\n#pagebreak()\n\n")
        } else {
          cat("\n\n")
        }
      })
    },
    error = identity,
    finally = {
      sink()
    }
  )
}

#' Compile Typst code to image files
#'
#' @param hts List of huxtables.
#' @param file Output file prefix.
#' @param open Automatically open the resulting files?
#' @param width Page width string or `NULL`.
#' @param height Page height string or `NULL`.
#' @param format Image format, e.g. "png" or "svg".
#' @param extra_args Additional command line arguments for typst.
#'
#' @noRd
do_quick_typst_images <- function(hts, file, open, width, height, format, extra_args = character()) {
  typst_file <- tempfile(fileext = ".typ")
  do_write_typst_file(hts, typst_file, width, height, page_break = TRUE)

  out_template <- if (length(hts) == 1L) {
    paste0(file, ".", format)
  } else {
    paste0(file, "-{0p}.", format)
  }
  args <- c("compile", typst_file, out_template, "--format", format, extra_args)

  if (Sys.which("typst") != "") {
    res <- system2("typst", args)
  } else if (Sys.which("quarto") != "") {
    res <- system2("quarto", c("typst", args))
  } else {
    stop("Could not find typst or quarto CLI. Please install typst or quarto to create ", toupper(format), "s.")
  }

  if (res != 0) {
    stop("Typst compilation failed")
  }
  if (!file.remove(typst_file)) warning("Could not remove intermediate Typst file '", typst_file, "'")

  if (open) {
    files <- list.files(dirname(file), pattern = paste0("^", basename(file), ".*\\.", format, "$"), full.names = TRUE)
    max_to_open <- min(5, length(files))
    if (length(files) > max_to_open) {
      warning("Opening just the first ", max_to_open, " of ", length(files), " files")
    }
    lapply(files[seq_len(max_to_open)], auto_open)
  }
  invisible(NULL)
}


confirm_prefix <- function(file) {
  if (!interactive()) stop("Please specify a `file` argument for non-interactive use of quick_typst_png().")
  dir <- dirname(file)
  if (!dir.exists(dir)) return(file)
  prefix <- basename(file)
  existing <- list.files(dir)
  existing <- existing[startsWith(existing, prefix)]
  if (length(existing) > 0) {
    answer <- readline(paste0("Files starting with '", file, "' already exist. Overwrite? [yN]"))
    if (!answer %in% c("y", "Y")) stop("OK, stopping")
  }
  file
}


confirm <- function(file) {
  if (!interactive()) stop("Please specify a `file` argument for non-interactive use of quick_xxx functions.")
  if (file.exists(file)) {
    answer <- readline(paste0("File '", file, "' already exists. Overwrite? [yN]"))
    if (!answer %in% c("y", "Y")) stop("OK, stopping")
  }
  file
}


auto_open <- function(path) {
  sysname <- Sys.info()["sysname"]
  safe_path <- shQuote(path)
  switch(sysname,
    Darwin  = system2("open", safe_path),
    Windows = system2("start", safe_path),
    Linux   = system2("xdg-open", safe_path),
    warning("Could not determine OS to open document automatically")
  )
}
