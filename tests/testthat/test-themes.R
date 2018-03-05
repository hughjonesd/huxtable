
context('Themes')


source('functions.R')


test_that('Themes work', {
  ht <- huxtable(a = 1:5, b = 1:5)
  expect_error(theme_basic(ht), regexp = NA)
  expect_error(theme_article(ht), regexp = NA)
  expect_error(theme_striped(ht), regexp = NA)
  expect_error(theme_plain(ht), regexp = NA)
})


test_that('Themes work with options', {
  ht <- huxtable(a = 1:5, b = 1:5)
  expect_error(theme_basic(ht, header_row = FALSE, header_col = FALSE), regexp = NA)
  expect_error(theme_striped(ht, stripe = 'purple', header_row = FALSE, header_col = FALSE), regexp = NA)
  expect_error(theme_article(ht, header_row = FALSE, header_col = FALSE), regexp = NA)
})
