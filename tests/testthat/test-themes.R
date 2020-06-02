
context("Themes")


test_that("Themes work", {
  ht <- huxtable(a = 1:5, b = 1:5)
  expect_error(theme_basic(ht), regexp = NA)
  expect_error(theme_article(ht), regexp = NA)
  expect_error(theme_striped(ht), regexp = NA)
  expect_silent(theme_bright(ht))
  expect_error(theme_plain(ht), regexp = NA)
  expect_error(theme_mondrian(ht), regexp = NA)
  expect_silent(theme_grey(ht))
  expect_silent(theme_blue(ht))
  expect_silent(theme_orange(ht))
  expect_silent(theme_green(ht))
})


test_that("Themes work with options", {
  ht <- huxtable(a = 1:5, b = 1:5, add_colnames = TRUE)
  expect_error(theme_basic(ht, header_rows = FALSE, header_cols = FALSE), regexp = NA)
  expect_error(theme_striped(ht, stripe = "purple", header_rows = FALSE, header_cols = FALSE),
        regexp = NA)
  expect_silent(theme_bright(ht, header_rows = FALSE, header_cols = TRUE))
  expect_error(theme_article(ht, header_rows = FALSE, header_cols = FALSE), regexp = NA)
  expect_silent(theme_compact(ht, header_rows = TRUE))
  expect_error(theme_mondrian(ht, prop_colored = 0.5), regexp = NA)
  expect_error(theme_mondrian(ht, prop_colored = 0), regexp = NA)
  expect_silent(theme_grey(ht, header_rows = FALSE, header_cols = TRUE))
  expect_silent(theme_blue(ht, header_rows = FALSE, header_cols = TRUE))
  expect_silent(theme_orange(ht, header_rows = FALSE, header_cols = TRUE))
  expect_silent(theme_green(ht, header_rows = FALSE, header_cols = TRUE))
})
