
context("End-to-end tests with standard converters")


skip_if_not_installed("knitr")
skip_if_not_installed("rmarkdown")
skip_without_pandoc()
require_temp_artefacts_dir()


validate_markdown <- function(md_string, output_format = "html_document") {
  force(output_format)
  on.exit({
    if (exists("tf")) try(file.remove(tf), silent = TRUE)
    if (exists("outfile")) try(file.remove(outfile), silent = TRUE)
  })

  tf <- tempfile(pattern = "markdown-example", fileext = ".md", tmpdir = "temp-artefacts")
  cat(md_string, file = tf)
  expect_silent(
    outfile <- rmarkdown::render(tf,
            output_format = output_format,
            output_file = NULL,
            output_dir = "temp-artefacts",
            intermediates_dir = "temp-artefacts",
            clean = TRUE,
            quiet = TRUE
          )
  ) # no error
}


test_render <- function(path, format, output_dir = "temp-artefacts") {
  output <- ""
  force(output_dir)
  on.exit(if (file.exists(output)) try(file.remove(output), silent = TRUE))
  expect_error(output <- rmarkdown::render(path,
          output_format     = format,
          quiet             = TRUE,
          output_dir        = output_dir,
          intermediates_dir = "temp-artefacts"
        ),
    regexp = NA,
    info   = list(path = path, format = format)
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
      output_files[i] <- knitr::knit(in_files[i], output = output_files[i], quiet = TRUE),
      regexp = NA
    )
    expect_true(file.exists(output_files[i]))
  }
})


test_that("guess_knitr_output_format() gets it right", {
  skip_on_cran()
  out <- character(0)
  on.exit(sapply(out, function (x) if (file.exists(x)) try(file.remove(x), silent = TRUE)))
  expect_silent(out[1] <- knitr::knit("guess-output-format-test.Rhtml", quiet = TRUE))
  expect_silent(out[2] <- knitr::knit("guess-output-format-test.Rnw", quiet = TRUE))

  test_render("guess-output-format-test-Rmd-html.Rmd", "html_document")
  test_render("guess-output-format-test-Rmd-pdf.Rmd", "pdf_document")

})


test_that("Four spaces does not cause <pre><code> markup", {
  output <- rmarkdown::render("fourspace-html-test.Rmd", output_dir = "temp-artefacts", quiet = TRUE)
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
  skip_if_not_installed("flextable") # skips on travis no-suggests where no vignettes

  test_render_all <- function (path) {
    test_render(path, "pdf_document")
    test_render(path, "html_document")
  }
  rmd_paths <- c("table-tester-2.Rmd")
  for (path in rmd_paths) {
    test_render_all(path)
  }

  # design-principles needs a CSV file, so we skip:
  rmd_filenames <- c("huxtable.Rmd", "huxreg.Rmd", "themes.Rmd")
  # this system.file may be devtools' patched version; these file paths are used in devtools::test:
  rmd_paths <- system.file("vignettes", rmd_filenames, package = "huxtable")
  if (! utils::file_test("-f", rmd_paths[1])) rmd_paths <-
         base::system.file("doc", rmd_filenames, package = "huxtable")
  if (! utils::file_test("-f", rmd_paths[1])) skip("Couldn't find vignettes...")
  for (path in rmd_paths) {
    file.copy(path, ".", overwrite = TRUE) # copy here so we can get the placeins-header.tex
    test_render_all(basename(path))
    file.remove(basename(path))
  }
})


test_that("Bookdown files", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("bookdown")
  test_render("bookdown-test.Rmd", "bookdown::pdf_book")
  test_render("bookdown-test.Rmd", "bookdown::html_book", output_dir = ".") # workaround a bug
})


test_that("Works with fontspec", {
  skip_on_cran()
  skip_on_os("linux") # no Arial
  oo <- options("huxtable.latex_use_fontspec")
  on.exit(options(oo))
  test_render("fontspec-test.Rmd", "pdf_document")
})


test_that("Word files", {
  skip_if_not_installed("flextable")
  skip_if_not(rmarkdown::pandoc_available("2.0.0"))
  test_render("word-test.Rmd", "word_document")
})
