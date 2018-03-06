

context("knit_print")


source('functions.R')

test_that("Can turn on and off printing of data frames", {
  expect_huxish <- function (dfr, bool) {
    tmp <- capture.output(knitr::knit_print(dfr))
    expect_identical(any(grepl("Column names", tmp)), bool)
  }
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_huxish(dfr, TRUE)
  options(huxtable.knit_print_df = FALSE)
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_huxish(dfr, FALSE)
  options(huxtable.knit_print_df = TRUE)
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_huxish(dfr, TRUE)
})

test_that("Can theme data frames", {
  options(huxtable.knit_print_df = TRUE)
  mytheme <- function (ht) {
    number_format(ht) <- 5
    ht
  }
  options(huxtable.knit_print_df_theme = mytheme)
  dfr <- data.frame(a = 1:2, b = 1:2)
  tmp <- capture.output(knitr::knit_print(dfr))
  expect_true(any(grepl("\\.00000", tmp)))
})
