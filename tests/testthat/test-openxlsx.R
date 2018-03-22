
context('openxlsx conversion')


source('functions.R')


test_that('Simple conversion works', {
  hx <- huxtable(a = 1:3, b = 4:6)
  expect_silent(wb <- as_Workbook(hx))
  expect_is(wb, 'Workbook')
})


test_that('Text properties work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  font(hx)[1, 1] <- 'Times New Roman'
  font_size(hx)[1, 2] <- 14
  bold(hx)[2, 1] <- TRUE
  italic(hx)[2, 2] <- TRUE
  text_color(hx)[3, 1] <- 'red'
  number_format(hx)[3, 2] <- '%3.2f'
  expect_silent(wb <- as_Workbook(hx))
})


test_that('Borders work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  top_border(hx)[1, ]    <- 1
  bottom_border(hx)[1, ] <- 2
  left_border(hx)[, 1]   <- 1
  right_border(hx)[, 2]  <- 1
  expect_silent(as_Workbook(hx))
})


test_that('Widths and alignment work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  col_width(hx) <- c(.7, .3)
  width(hx) <- .9
  expect_silent(as_Workbook(hx))
  row_height(hx) <- c(.2, .6, .2)
  height(hx) <- .8
  expect_silent(as_Workbook(hx))
  align(hx)[1, 1]  <- 'right'
  valign(hx)[1, 2] <- 'middle'
  align(hx)[2, 1]  <- 'centre'
  expect_silent(as_Workbook(hx))
})


test_that('Captions work', {
  hx <- huxtable(a = 1:3, b = 4:6)
  caption(hx) <- 'Caption here'
  for (pos in c('top', 'topleft', 'topcentre', 'topright', 'bottom', 'bottomleft', 'bottomcentre', 'bottomright')) {
    caption_pos(hx) <- pos
    expect_silent(as_Workbook(hx))
  }
})


test_that('Can add to an existing workbook', {
  hx <- huxtable(a = 1:3, b = 4:6)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
  expect_silent(as_Workbook(hx, Workbook = wb, sheet = 'Another sheet'))
})

test_that('Works for single-column huxtables with and without row names', {
  hx <- huxtable(1, 2, 3)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
  hx <- huxtable(a = 1, b = 2, c = 3)
  wb <- openxlsx::createWorkbook()
  expect_silent(wb <- as_Workbook(hx, Workbook = wb))
})
