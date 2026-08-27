skip_if_not_installed("knitr")
skip_if_not_installed("rmarkdown")
skip_without_pandoc()
require_temp_artefacts_dir()


oo_al <- options("huxtable.autolabel" = FALSE)


# Note: This test file sets autolabel = FALSE and cleans up libs directory
# The option will be restored when the test session ends


validate_markdown <- function(md_string) {
  on.exit({
    try(file.remove(tf), silent = TRUE)
    try(file.remove(outfile), silent = TRUE)
  })

  tf <- tempfile(
    pattern = "markdown-example",
    fileext = ".md",
    tmpdir = "temp-artefacts"
  )
  cat(md_string, file = tf)

  expect_silent(
    outfile <- rmarkdown::render(tf,
      output_format = rmarkdown::html_document(
        pandoc_args = c(
          "--metadata",
          "title=\"Avoid warnings\""
        )
      ),
      output_file = NULL,
      output_dir = "temp-artefacts",
      intermediates_dir = "temp-artefacts",
      clean = TRUE,
      quiet = TRUE
    )
  )
}


test_render <- function(path, format, output_dir = "temp-artefacts") {
  output <- ""
  force(output_dir)
  oo <- options(huxtable.latex_use_fontspec = FALSE)
  on.exit({
    options(oo)
    if (file.exists(output)) try(file.remove(output), silent = TRUE)
  })
  expect_error(
    output <- rmarkdown::render(path,
      output_format     = format,
      quiet             = TRUE,
      output_dir        = output_dir,
      intermediates_dir = "temp-artefacts"
    ),
    regexp = NA,
    info = list(path = path, format = format)
  )
}

# for some reason if this comes after the knitting to PDF files, it crashes on the third md
# ... knitr somehow ends up with a huge vector of information
test_that("to_md produces valid markdown", {
  ht <- hux(a = 1:5, b = 1:5)
  md <- to_md(ht)
  validate_markdown(md)
  ht <- set_all_borders(ht, 1)
  md <- to_md(ht)
  validate_markdown(md)
  ht <- set_caption(ht, "Some caption")
  md <- to_md(ht)
  validate_markdown(md)
  expect_match(md, "Some caption", fixed = TRUE)
})


test_that("Can knit .Rhtml files to HTML", {
  in_files <- "knitr-HTML-tester.Rhtml"
  output_files <- paste0(sub("\\.Rhtml", "", in_files), ".html")
  output_files <- file.path("temp-artefacts", output_files)
  on.exit(try(file.remove(output_files), silent = TRUE))
  for (i in seq_along(in_files)) {
    expect_error(
      output_files[i] <- knitr::knit(in_files[i],
        output = output_files[i],
        quiet = TRUE
      ),
      regexp = NA
    )
    expect_true(file.exists(output_files[i]))
  }
})


test_that("guess_knitr_output_format() gets it right", {
  skip_on_cran()
  out <- character(0)
  on.exit({
    lapply(out, function(x) {
      if (file.exists(x)) try(file.remove(x), silent = TRUE)
    })
  })
  expect_silent(out[1] <- knitr::knit("guess-output-format-test.Rhtml",
    quiet = TRUE
  ))
  expect_silent(out[2] <- knitr::knit("guess-output-format-test.Rnw",
    quiet = TRUE
  ))

  test_render("guess-output-format-test-Rmd-html.Rmd", "html_document")
  test_render("guess-output-format-test-Rmd-pdf.Rmd", "pdf_document")
})


test_that("Four spaces does not cause <pre><code> markup", {
  output <- rmarkdown::render("fourspace-html-test.Rmd",
    output_dir = "temp-artefacts", quiet = TRUE
  )
  on.exit(if (exists("output")) try(file.remove(output), silent = TRUE))
  lines <- readLines(output)
  expect_false(any(grepl("findme&lt;/td&gt;", lines)))
})


test_that("Row heights do not screw up LaTeX multicol", {
  skip_on_cran()
  skip_on_travis() # temporary

  test_render("rowheight-multicol-test.Rmd", NULL)
})


test_that("echo = TRUE does not cause option clash", {
  skip_on_cran()
  test_render("echo-true-latex-test.Rmd", NULL)
})


test_that("Various Rmd files render without errors", {
  skip_on_cran()

  test_render_all <- function(path) {
    test_render(path, "pdf_document")
    test_render(path, "html_document")
  }

  rmd_paths <- c("table-tester-2.Rmd")
  for (path in rmd_paths) {
    test_render_all(path)
  }
})


test_that("Bookdown files", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("bookdown")
  test_render("bookdown-test.Rmd", "bookdown::pdf_book")
  # output_dir = "." works around a bug:
  test_render("bookdown-test.Rmd", "bookdown::html_book", output_dir = ".")
})


test_that("Word files", {
  skip_if_not_installed("flextable")
  skip_if_not(rmarkdown::pandoc_available("2.0.0"))
  test_render("word-test.Rmd", "word_document")
})


test_that("quarto files", {
  skip_if_not_installed("quarto")
  qp <- quarto::quarto_path()
  skip_if_not(is.character(qp))

  on.exit({
    for (f in c("quarto-test-out.pdf", "quarto-test-out.html")) {
      if (file.exists(f)) try(file.remove(f), silent = TRUE)
    }
    if (file.exists("quarto-test_files")) {
      try(unlink("quarto-test_files", recursive = TRUE), silent = TRUE)
    }
  })

  if (quarto::quarto_version() < "1.4") {
    expect_silent(
      quarto::quarto_render("quarto-test.qmd",
        output_format = "pdf",
        output_file = "quarto-test-out.pdf",
        execute_dir = "temp-artefacts",
        debug = FALSE, quiet = TRUE
      )
    )
  } else {
    # for some reason (probably due to use of processx::) I can't
    # capture the specific huxtable error from make_label() here
    expect_error(
      quarto::quarto_render("quarto-test.qmd",
        output_format = "pdf",
        output_file = "quarto-test-out.pdf",
        execute_dir = "temp-artefacts",
        debug = FALSE, quiet = TRUE
      )
    )
    expect_silent(
      quarto::quarto_render("quarto-test-tex-labels.qmd",
        output_format = "pdf",
        output_file = "quarto-test-tex-labels-out.pdf",
        execute_dir = "temp-artefacts",
        debug = FALSE, quiet = TRUE
      )
    )
  }

  expect_silent(
    quarto::quarto_render("quarto-test.qmd",
      output_format = "html",
      output_file = "quarto-test-out.html",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
})


quarto_typst_is_valid <- function(qmd, output_dir = "temp-artefacts") {
  args <- c("render", qmd, "--to", "typst", "--output-dir", output_dir, "--debug")
  res <- system2("quarto", args)
  if (!identical(res, 0L)) {
    return(FALSE)
  }
  typ_file <- file.path(dirname(qmd), sub("\\.qmd$", ".typ", basename(qmd)))
  if (!file.exists(typ_file)) {
    return(FALSE)
  }
  res2 <- system2(
    "quarto",
    c("typst", "compile", typ_file, "/dev/null", "--format", "pdf")
  )
  identical(res2, 0L)
}

test_that("quarto typst output is valid", {
  skip_on_cran()
  if (Sys.which("quarto") == "") skip("quarto CLI not found")
  require_temp_artefacts_dir()
  on.exit(
    {
      for (f in c("quarto-typst.typ", file.path("temp-artefacts", "quarto-typst.pdf"))) {
        if (file.exists(f)) try(file.remove(f), silent = TRUE)
      }
    },
    add = TRUE
  )
  expect_true(quarto_typst_is_valid("quarto-typst.qmd", "temp-artefacts"))
})


test_that("breakable LaTeX tables compile across pages", {
  skip_on_cran()
  required_tools <- Sys.which(c("pdflatex", "pdfinfo", "pdftotext"))
  skip_if(any(required_tools == ""), "TeX and Poppler command-line tools are required")

  top <- hux(
    c("Heading", sprintf("top row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(top)[1] <- TRUE
  breakable(top) <- TRUE
  caption(top) <- "Top caption"
  label(top) <- "tab:breakable-top"

  bottom <- hux(
    c("Heading", sprintf("bottom row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(bottom)[1] <- TRUE
  breakable(bottom) <- TRUE
  caption(bottom) <- "Bottom caption"
  caption_pos(bottom) <- "bottom"
  label(bottom) <- "tab:breakable-bottom"

  tex_dir <- tempfile("breakable-latex-")
  dir.create(tex_dir)
  old_wd <- setwd(tex_dir)
  on.exit(setwd(old_wd), add = TRUE)
  tex <- paste0(
    "\\documentclass{article}\n",
    report_latex_dependencies(quiet = TRUE, as_string = TRUE),
    "\\begin{document}\n",
    "References: \\ref{tab:breakable-top} and \\ref{tab:breakable-bottom}.\n",
    to_latex(top), "\n", to_latex(bottom),
    "\n\\end{document}\n"
  )
  writeLines(tex, "breakable.tex")

  latex_args <- c("-interaction=nonstopmode", "-halt-on-error", "breakable.tex")
  first_run <- system2(required_tools[["pdflatex"]], latex_args, stdout = TRUE, stderr = TRUE)
  second_run <- system2(required_tools[["pdflatex"]], latex_args, stdout = TRUE, stderr = TRUE)
  expect_null(attr(first_run, "status"), info = paste(first_run, collapse = "\n"))
  expect_null(attr(second_run, "status"), info = paste(second_run, collapse = "\n"))
  expect_false(any(grepl("Float too large", second_run, fixed = TRUE)))
  expect_false(any(grepl("undefined references", second_run, ignore.case = TRUE)))

  pdf_info <- system2(required_tools[["pdfinfo"]], "breakable.pdf", stdout = TRUE)
  page_count <- as.integer(sub("Pages:[[:space:]]+", "", grep("^Pages:", pdf_info, value = TRUE)))
  expect_gt(page_count, 1L)

  expect_equal(
    system2(required_tools[["pdftotext"]], c("-layout", "breakable.pdf", "breakable.txt")),
    0L
  )
  pdf_text <- paste(readLines("breakable.txt", warn = FALSE), collapse = "\n")
  pdf_pages <- strsplit(pdf_text, "\f", fixed = TRUE)[[1]]
  nonempty_pages <- which(nzchar(trimws(pdf_pages)))
  expect_equal(which(grepl("Top caption", pdf_pages, fixed = TRUE)), 1L)
  expect_equal(which(grepl("Bottom caption", pdf_pages, fixed = TRUE)), max(nonempty_pages))
  expect_lt(regexpr("Top caption", pdf_text, fixed = TRUE), regexpr("top row 001", pdf_text, fixed = TRUE))
  expect_lt(regexpr("bottom row 120", pdf_text, fixed = TRUE), regexpr("Bottom caption", pdf_text, fixed = TRUE))
  expect_gte(length(gregexpr("Heading", pdf_text, fixed = TRUE)[[1]]), page_count)
  expect_false(grepl("??", pdf_text, fixed = TRUE))
})


test_that("breakable Typst tables compile across pages", {
  skip_on_cran()
  required_tools <- Sys.which(c("typst", "pdfinfo", "pdftotext"))
  skip_if(any(required_tools == ""), "Typst and Poppler command-line tools are required")

  ht <- hux(
    c("Heading", sprintf("row %03d", 1:120)),
    c("Value", 1:120),
    add_colnames = FALSE
  )
  header_rows(ht)[1] <- TRUE
  breakable(ht) <- TRUE
  caption(ht) <- "Typst caption"
  label(ht) <- "tab:breakable-typst"

  typ_dir <- tempfile("breakable-typst-")
  dir.create(typ_dir)
  typ_file <- file.path(typ_dir, "breakable.typ")
  pdf_file <- file.path(typ_dir, "breakable.pdf")
  text_file <- file.path(typ_dir, "breakable.txt")
  writeLines(c(to_typst(ht), "Reference: @tab:breakable-typst"), typ_file)

  typst_run <- system2(
    required_tools[["typst"]],
    c("compile", typ_file, pdf_file),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(typst_run, "status"), info = paste(typst_run, collapse = "\n"))

  pdf_info <- system2(required_tools[["pdfinfo"]], pdf_file, stdout = TRUE)
  page_count <- as.integer(sub("Pages:[[:space:]]+", "", grep("^Pages:", pdf_info, value = TRUE)))
  expect_gt(page_count, 1L)
  expect_equal(system2(required_tools[["pdftotext"]], c("-layout", pdf_file, text_file)), 0L)
  pdf_text <- paste(readLines(text_file, warn = FALSE), collapse = "\n")
  expect_gte(length(gregexpr("Heading", pdf_text, fixed = TRUE)[[1]]), page_count)
  expect_false(grepl("??", pdf_text, fixed = TRUE))
})


test_that("Works with fontspec", {
  skip_on_cran()
  skip_on_os("linux") # no Arial
  oo <- options("huxtable.latex_use_fontspec")
  on.exit(options(oo))
  test_render("fontspec-test.Rmd", "pdf_document")
})


test_that("huxtable.long_minus", {
  oo <- options(huxtable.long_minus = TRUE)
  on.exit(options(oo))

  ht <- hux(c("1", "-1.5"), c("1e-3", "1e3"), c("1", "-1e-3"))
  expect_silent(to_screen(ht))
  expect_silent(to_md(ht))

  expect_silent(quick_html(ht,
    file = file.path("temp-artefacts", "long-minus-test.html"), open = FALSE
  ))
  expect_silent(quick_latex(ht,
    file = file.path("temp-artefacts", "long-minus-test.tex"), open = FALSE
  ))
  expect_silent(quick_rtf(ht,
    file = file.path("temp-artefacts", "long-minus-test.rtf"), open = FALSE
  ))

  skip_if_not_installed("openxlsx")
  expect_silent(quick_xlsx(ht,
    file = file.path("temp-artefacts", "long-minus-test.xlsx"), open = FALSE
  ))

  skip_if_not_installed("flextable")
  expect_silent(quick_docx(ht,
    file = file.path("temp-artefacts", "long-minus-test.docx"), open = FALSE
  ))
  expect_silent(quick_pptx(ht,
    file = file.path("temp-artefacts", "long-minus-test.pptx"), open = FALSE
  ))

  skip_on_cran()
  expect_silent(quick_pdf(ht,
    file = file.path("temp-artefacts", "long-minus-test.pdf"), open = FALSE
  ))
})



test_that("huxtable.latex_siunitx_align", {
  oo <- options(huxtable.latex_siunitx_align = TRUE)
  on.exit(options(oo))

  ht <- hux(c("1", "-1.5"), c("1.2e-3", "1.33e3"), c("1", "-1e-3"))
  expect_silent(to_screen(ht))
  expect_silent(to_md(ht))

  skip_on_cran()
  test_render("siunitx-test.Rmd", "pdf_document")
})
