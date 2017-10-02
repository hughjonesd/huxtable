
context('flextable conversion')


source('functions.R')


test_that('Simple conversion works', {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_error(ft <- as_flextable(hx), regexp = NA)
  expect_is(ft, 'flextable')
})


test_that('Text properties work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  font(hx)[1, 1] <- 'Times'
  font_size(hx)[1, 2] <- 14
  bold(hx)[2, 1] <- TRUE
  italic(hx)[2, 2] <- TRUE
  text_color(hx)[3, 1] <- 'red'
  number_format(hx)[3, 2] <- '%3.2f'
  expect_error(as_flextable(hx), regexp = NA)
})


test_that('Borders work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  top_border(hx)[1, ]    <- 1
  bottom_border(hx)[1, ] <- 2
  left_border(hx)[, 1]   <- 1
  right_border(hx)[, 2]  <- 1
  expect_error(as_flextable(hx), regexp = NA)
})
