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


test_quarto_render <- function(...) {
  old_r_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit({
    if (is.na(old_r_libs)) {
      Sys.unsetenv("R_LIBS")
    } else {
      Sys.setenv(R_LIBS = old_r_libs)
    }
  })
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  quarto::quarto_render(...)
}


test_that("quarto files", {
  skip_if_not_installed("quarto")
  qp <- quarto::quarto_path()
  skip_if_not(is.character(qp))

  on.exit({
    for (f in c(
      "quarto-test-out.pdf", "quarto-test-out.html",
      "quarto-test-tex-labels-out.pdf"
    )) {
      if (file.exists(f)) try(file.remove(f), silent = TRUE)
    }
    if (file.exists("quarto-test_files")) {
      try(unlink("quarto-test_files", recursive = TRUE), silent = TRUE)
    }
  })

  expect_silent(
    test_quarto_render("quarto-test.qmd",
      output_format = "pdf",
      output_file = "quarto-test-out.pdf",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
  expect_silent(
    test_quarto_render("quarto-test-tex-labels.qmd",
      output_format = "pdf",
      output_file = "quarto-test-tex-labels-out.pdf",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )

  expect_silent(
    test_quarto_render("quarto-test.qmd",
      output_format = "html",
      output_file = "quarto-test-out.html",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
})


test_that("Quarto captions and labels override huxtable output", {
  skip_if_not_installed("quarto")
  skip_if_not(is.character(quarto::quarto_path()))

  outputs <- c(
    "quarto-caption-precedence.html",
    "quarto-caption-precedence.pdf",
    "quarto-caption-precedence.tex",
    "quarto-caption-precedence.typ",
    "quarto-caption-precedence.docx"
  )
  on.exit({
    for (f in outputs) if (file.exists(f)) try(file.remove(f), silent = TRUE)
    if (file.exists("quarto-caption-precedence_files")) {
      try(unlink("quarto-caption-precedence_files", recursive = TRUE), silent = TRUE)
    }
  })

  expect_silent(
    test_quarto_render("quarto-caption-precedence.qmd",
      output_format = "html",
      output_file = "quarto-caption-precedence.html",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
  html <- paste(readLines("quarto-caption-precedence.html", warn = FALSE), collapse = "\n")
  expect_match(html, "Caption from Quarto", fixed = TRUE)
  expect_match(html, 'id="tbl-quarto"', fixed = TRUE)
  expect_match(html, 'href="#tbl-quarto"', fixed = TRUE)
  expect_false(grepl("Caption from Huxtable", html, fixed = TRUE))
  expect_false(grepl("tbl-huxtable", html, fixed = TRUE))

  expect_silent(
    test_quarto_render("quarto-caption-precedence.qmd",
      output_format = "pdf",
      output_file = "quarto-caption-precedence.pdf",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
  tex <- paste(readLines("quarto-caption-precedence.tex", warn = FALSE), collapse = "\n")
  expect_match(tex, "Caption from Quarto", fixed = TRUE)
  expect_match(tex, "\\label{tbl-quarto}", fixed = TRUE)
  expect_match(tex, "\\begin{longtable}", fixed = TRUE)
  expect_match(tex, "Breakable caption from Quarto", fixed = TRUE)
  expect_match(tex, "\\label{tbl-long}", fixed = TRUE)
  expect_false(grepl("caption from Huxtable", tex, ignore.case = TRUE))
  expect_false(grepl("tbl-huxtable", tex, fixed = TRUE))

  expect_silent(
    test_quarto_render("quarto-caption-precedence.qmd",
      output_format = "typst",
      output_file = "quarto-caption-precedence.pdf",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
  typ <- paste(readLines("quarto-caption-precedence.typ", warn = FALSE), collapse = "\n")
  expect_match(typ, "Caption from Quarto", fixed = TRUE)
  expect_match(typ, "<tbl-quarto>", fixed = TRUE)
  expect_match(typ, "#ref(<tbl-quarto>", fixed = TRUE)
  expect_false(grepl("Caption from Huxtable", typ, fixed = TRUE))
  expect_false(grepl("tbl-huxtable", typ, fixed = TRUE))

  skip_if_not_installed("flextable")
  expect_silent(
    test_quarto_render("quarto-caption-precedence.qmd",
      output_format = "docx",
      output_file = "quarto-caption-precedence.docx",
      execute_dir = "temp-artefacts",
      debug = FALSE, quiet = TRUE
    )
  )
  docx_dir <- tempfile("quarto-docx-")
  dir.create(docx_dir)
  on.exit(unlink(docx_dir, recursive = TRUE), add = TRUE)
  utils::unzip(
    "quarto-caption-precedence.docx",
    files = "word/document.xml",
    exdir = docx_dir
  )
  word_xml <- paste(
    readLines(file.path(docx_dir, "word/document.xml"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(word_xml, "Caption from Quarto", fixed = TRUE)
  expect_match(word_xml, "tbl-quarto", fixed = TRUE)
  expect_false(grepl("Caption from Huxtable", word_xml, fixed = TRUE))
  expect_false(grepl("tbl-huxtable", word_xml, fixed = TRUE))
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
