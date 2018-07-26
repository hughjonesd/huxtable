
context('flextable conversion')


test_that('Simple conversion works', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  expect_error(ft <- as_flextable(hx), regexp = NA)
  expect_is(ft, 'flextable')
})


test_that('Text properties work', {
  skip_if_not_installed('flextable')

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
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  top_border(hx)[1, ]    <- 1
  bottom_border(hx)[1, ] <- 2
  left_border(hx)[, 1]   <- 1
  right_border(hx)[, 2]  <- 1
  expect_error(as_flextable(hx), regexp = NA)
})


test_that('background colour works', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  background_color(hx)[1:2,] <- 'yellow'
  expect_silent(as_flextable(hx))
})


test_that('merged cells work', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  colspan(hx)[1, 1] <- 2
  rowspan(hx)[2, 1] <- 2
  expect_silent(as_flextable(hx))
})


test_that('row heights and column widths work', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  row_height(hx) <- c(.5, .25, .25)
  col_width(hx) <- c(.6, .4)
  expect_silent(as_flextable(hx))
})


test_that('colnames_to_header argument', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  expect_error(as_flextable(hx, colnames_to_header = FALSE), regexp = NA)
  expect_error(as_flextable(hx, colnames_to_header = TRUE), regexp = NA)
})


test_that('rotation works', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  rotation(hx)[1, 1] <- 90
  expect_silent(as_flextable(hx))
  rotation(hx)[1, 1] <- 45
  expect_warning(as_flextable(hx), "can only handle rotation")
})


test_that('as_FlexTable gives warning', {
  skip_if_not_installed('flextable')

  hx <- huxtable(a = 1:3, b = 4:6)
  expect_warning(ft <- as_FlexTable(hx), 'deprecated')
  expect_is(ft, 'flextable')
})


test_that('0-row/0-column huxtables work', {
  skip_if_not_installed('flextable')

  h_nrow0 <- hux(a = character(0), b = character(0), add_colnames = FALSE)
  h_ncol0 <- hux(a = 1:2)[, FALSE]
  skip("0-length tables don't work in flextable yet")
  expect_warning(as_flextable(h_nrow0), "row")
  expect_warning(as_flextable(h_ncol0), "col")
})
