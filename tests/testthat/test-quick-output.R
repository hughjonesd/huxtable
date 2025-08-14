skip_without_latex_deps <- function() {
  skip_on_cran() # CRAN doesn't play nicely with tinytex, it uses full texlive
  skip_if_not(
    requireNamespace("tinytex", quietly = TRUE) ||
      check_latex_dependencies(quiet = TRUE)
  )
}


skip_without_typst <- function() {
  if (Sys.which("typst") == "" && Sys.which("quarto") == "") skip("typst CLI not found")
}


test_that("Quick output functions create files", {

  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)

  tf <- tempfile(fileext = ".htm")
  expect_silent(quick_html(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = ".rtf")
  expect_silent(quick_rtf(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = ".tex")
  expect_silent(quick_latex(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))

  skip_without_typst()
  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_typst_pdf(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))
  
 
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("flextable")
  
  tf <- tempfile(fileext = ".docx")
  expect_silent(quick_docx(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))
  
  tf <- tempfile(fileext = ".pptx")
  expect_silent(quick_pptx(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = ".xlsx")
  expect_silent(quick_xlsx(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))
})


test_that("quick_latex can be compiled", {
  skip_on_appveyor() # no pdflatex
  skip_on_ci() # trouble on github
  skip_without_latex_deps()

  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile(fileext = ".tex")
  expect_silent(quick_latex(m, dfr, ht, file = tf, open = FALSE))
  try(tools::texi2pdf(tf, clean = TRUE))
  output_file <- sub("\\.tex$", ".pdf", basename(tf))
  expect_true(file.exists(output_file))
  try(file.remove(output_file), silent = TRUE)
})


test_that("quick_pdf works", {
  skip_on_appveyor()
  skip_without_latex_deps()

  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_pdf(m, dfr, ht, file = tf))
  expect_true(file.exists(tf))
})


test_that("quick_pdf works with height and width options", {
  skip_on_appveyor()
  skip_without_latex_deps()

  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)

  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_pdf(m, dfr, ht, file = tf, height = "4in"))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_pdf(m, dfr, ht, file = tf, width = "4in"))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_pdf(m, dfr, ht, file = tf, height = "4in", width = "4in"))
  expect_true(file.exists(tf))
})


test_that("quick_typst creates files", {
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile(fileext = ".typ")
  expect_silent(quick_typst(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))
})


test_that("quick_typst_pdf works", {
  skip_without_typst()
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile(fileext = ".pdf")
  expect_silent(quick_typst_pdf(m, dfr, ht, file = tf, open = FALSE))
  expect_true(file.exists(tf))
})


test_that("quick_typst_png works", {
  skip_without_typst()
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile()
  expect_silent(quick_typst_png(m, dfr, ht, file = tf, open = FALSE))
  files <- list.files(dirname(tf), pattern = paste0("^", basename(tf), ".*\\.png$"), full.names = TRUE)
  expect_equal(length(files), 3)
})

test_that("quick_typst_svg works", {
  skip_without_typst()
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile()
  expect_silent(quick_typst_svg(m, dfr, ht, file = tf, open = FALSE))
  files <- list.files(dirname(tf), pattern = paste0("^", basename(tf), ".*\\.svg$"), full.names = TRUE)
  expect_equal(length(files), 3)
})


test_that("Quick output functions stop if called non-interactively with no `file` argument", {
  skip_if(interactive())

  ht <- hux(a = 1:2, b = 1:2)
  expect_error(quick_html(ht))
  expect_false(file.exists("huxtable-output.html"))
})
