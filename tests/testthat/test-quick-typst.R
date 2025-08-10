skip_without_typst <- function() {
  if (Sys.which("typst") == "") skip("typst CLI not found")
}


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

