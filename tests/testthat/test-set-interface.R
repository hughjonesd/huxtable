
context('Row and column functions, set_* interface')


test_that('Row/column function examples unchanged', {
  test_ex_same('every')
  test_ex_same('final')
})

test_that('final', {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
  expect_equivalent(final(2)(dfr, 1), 4:5)
  expect_equivalent(final(2)(dfr, 2), 3:4)
  expect_equivalent(final(6)(dfr, 1), 1:5)
})


test_that('every(), evens and odds', {
  dfr <- data.frame(a = 1:7, b = 1:7, d = 1:7, e = 1:7)
  expect_equivalent(every(3, from = 1)(dfr, 1), c(1, 4, 7))
  expect_equivalent(every(3, from = 1)(dfr, 2), c(1, 4))
  expect_equivalent(evens(dfr, 1), c(2, 4, 6))
  expect_equivalent(evens(dfr, 2), c(2, 4))
  expect_equivalent(odds(dfr, 1), c(1, 3, 5, 7))
  expect_equivalent(odds(dfr, 2), c(1, 3))
  expect_equivalent(everywhere(dfr, 1), 1:7)
  expect_equivalent(everywhere(dfr, 2), 1:4)
})


test_that('set_cell_properties', {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  ht2 <- set_cell_properties(ht, 1, 1, font = 'times', align = 'right')
  expect_equivalent(font(ht2)[1, 1], 'times')
  expect_equivalent(align(ht2)[1, 1], 'right')
  r <- 2
  c <- 1
  ht3 <- set_cell_properties(ht, r, c, bold = TRUE)
  expect_equivalent(bold(ht3)[2, 1], TRUE)

})


test_that('set_cell_properties fails with bad arguments', {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  expect_error(ht <- set_cell_properties(ht, 1, 1, bad = 'no!'))
})


test_that('set_* works with variables as arguments', {
  ht_orig <- hux(a = 1:2, b = 1:2)
  rownum <- 2
  colnum <- 1
  ht2 <- set_bold(ht_orig, rownum, colnum, TRUE)
  expect_equivalent(bold(ht2), matrix(c(FALSE, TRUE, FALSE, FALSE), 2, 2))
  boldness <- TRUE
  ht3 <- set_bold(ht_orig, 1:2, 1:2, boldness)
  expect_equivalent(bold(ht3), matrix(TRUE, 2, 2))
})

test_that('set_* works with cell functions', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, evens, 1:2, 'times')
  ht <- set_font(ht, odds, 1:2, 'palatino')
  expect_equivalent(font(ht), matrix(c('palatino', 'times'), 4, 2))
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, every(1), evens, 'times')
  ht <- set_font(ht, every(1), odds, 'palatino')
  expect_equivalent(font(ht), matrix(c('palatino', 'times'), 4, 2, byrow = TRUE))
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, every(3, from = 1), every(1), 'times')
  expect_equivalent(font(ht), matrix(c('times', NA, NA, 'times'), 4, 2))
})


test_that('set_* works with row and column functions', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_col_width(ht, evens, '20pt')
  ht <- set_col_width(ht, odds, '40pt')
  ht <- set_row_height(ht, evens, '15pt')
  ht <- set_row_height(ht, odds, '30pt')
  expect_equivalent(col_width(ht), c('40pt', '20pt'))
  expect_equivalent(row_height(ht), rep(c('30pt', '15pt'), 2))
})


test_that('set_* works with byrow', {
  ht <- hux(a = 1:2, b = 1:2)
  ht <- set_font(ht, 1:2, 1:2, c('times', 'palatino'), byrow = TRUE)
  expect_equivalent(font(ht), matrix(c('times', 'times', 'palatino', 'palatino'), 2, 2))

  ht <- hux(a = 1:2, b = 1:2, d = 1:2)
  ht <- set_font(ht, 2, 1:2, c('times', 'palatino'), byrow = TRUE)
  expect_equivalent(font(ht), matrix(c(NA, 'times', NA, 'palatino', NA, NA), 2, 3))

  ht <- hux(a = 1:2, b = 1:2)
  ht <- set_font(ht, c('times', 'palatino'), byrow = TRUE)
  expect_equivalent(font(ht), matrix(c('times', 'times', 'palatino', 'palatino'), 2, 2))
})


test_that('set_*: 3 argument form', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_font(ht, where(ht > 0), value = 'times')
  expect_equivalent(font(ht2), matrix(c('times', NA, NA, 'times'), 2, 2))
  ht3 <- set_font(ht, where(ht > 0), 'times')
  expect_equivalent(font(ht3), matrix(c('times', NA, NA, 'times'), 2, 2))
  expect_error(set_font(ht, 1:3, 'times')) # no cols!
  expect_error(set_font(ht, where(ht > 0), 'times', byrow = TRUE)) # hard to interpret
})


test_that('set_*: 2 argument form', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_font(ht, 'times')
  expect_equivalent(font(ht2), matrix('times', 2, 2))
  ht3 <- set_font(ht, value = 'times')
  expect_equivalent(font(ht3), matrix('times', 2, 2))
  ht4 <- set_font(ht, value = c('times', 'arial'), byrow = TRUE)
  expect_equivalent(font(ht4), matrix(c('times', 'times', 'arial', 'arial'), 2, 2))
  ht5 <- set_font(ht, c('times', 'arial'), byrow = TRUE)
  expect_equivalent(font(ht5), matrix(c('times', 'times', 'arial', 'arial'), 2, 2))
  ht6 <- set_col_width(ht, c(.6, .4))
  expect_equivalent(col_width(ht6), c(.6, .4))
  ht7 <- set_row_height(ht, c(.6, .4))
  expect_equivalent(row_height(ht7), c(.6, .4))
})


test_that('set_* works with row and col "empty"', {
  ht_orig <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_font(ht_orig, 1, everywhere, 'times')
  expect_equivalent(font(ht2), matrix(c('times', NA), 2, 2))
  ht3 <- set_font(ht_orig, everywhere, 1, 'times')
  expect_equivalent(font(ht3), matrix(c('times', 'times', NA, NA), 2, 2))
  ht4 <- set_font(ht_orig, 'times')
  expect_equivalent(font(ht4), matrix('times', 2, 2))
})


test_that('set_all_*', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_all_borders(ht, 1)
  expect_equivalent(top_border(ht2), matrix(1, 2, 2))
  ht3 <- set_all_borders(ht, where(ht > 0), 1)
  expect_equivalent(top_border(ht3), matrix(c(1, 0, 0, 1), 2, 2))
  ht4 <- set_all_borders(ht, 1, 2, 1)
  expect_equivalent(top_border(ht4), matrix(c(0, 0, 1, 0), 2, 2))
  rownum <- 1
  colnum <- 2
  ht5 <- set_all_borders(ht, rownum, colnum, 1)
  expect_equivalent(top_border(ht5), matrix(c(0, 0, 1, 0), 2, 2))
  border_size <- 2
  ht6 <- set_all_borders(ht, border_size)
  expect_equivalent(top_border(ht6), matrix(border_size, 2, 2))

  ht7 <- set_all_borders(ht, 1:2, tidyselect::matches('a|b'), 1)
  expect_equivalent(top_border(ht7), matrix(1, 2, 2))
})


test_that('map_all_*', {
  ht <- huxtable(1:5, 5:1)

  ht2 <- map_all_borders(ht, by_ranges(3, c(0, 1)))
  expect_equivalent(left_border(ht2), 1 * (as.matrix(ht2) >= 3))
  expect_equivalent(right_border(ht2), 1 * (as.matrix(ht2) >= 3))

  ht3 <- map_all_border_colors(ht, by_ranges(3, c('red', 'black')))
  expected <- matrix(ifelse(as.matrix(ht) >= 3, 'black', 'red'), 5, 2)
  expect_equivalent(left_border_color(ht3), expected)
  expect_equivalent(right_border_color(ht3), expected)

  ht4 <- map_all_border_styles(ht, by_ranges(3, c('solid', 'double')))
  expected <- matrix(ifelse(as.matrix(ht) >= 3, 'double', 'solid'), 5, 2)
  expect_equivalent(left_border_style(ht4), expected)
  expect_equivalent(right_border_style(ht4), expected)

  ht5 <- map_all_padding(ht, by_ranges(3, c(0, 10)))
  expect_equivalent(left_padding(ht5), 10 * (as.matrix(ht) >= 3))
  expect_equivalent(right_padding(ht5), 10 * (as.matrix(ht) >= 3))

  ht6 <- map_all_borders(ht, 1:2, 1:2, by_ranges(3, c(2, 4)))
  expected <- matrix(c(2, 2, 0, 0, 0, 4, 4, 0, 0, 0), 5, 2)
  expect_equivalent(left_border(ht6), expected)
  expect_equivalent(right_border(ht6), expected)
})


test_that('set_all_* functions work when huxtable is not attached', {
  # NB as written this test can only be run from the command line; detach call silently fails
  library(huxtable)
  detach(package:huxtable)
  ht <- huxtable::hux(a = c(1, 0), b = c(0, 1))
  expect_silent(ht2 <- huxtable::set_all_borders(ht, 1))
  expect_silent(ht3 <- huxtable::set_all_border_colors(ht, 'red'))
  expect_silent(ht4 <- huxtable::set_all_padding(ht, 1))
  expect_silent(ht5 <- huxtable::set_all_border_styles(ht, 'double'))
  library(huxtable) # we reattach before these tests, or we have problems with unavailable methods
  expect_equivalent(top_border(ht2), matrix(1, 2, 2))
  expect_equivalent(top_border_color(ht3), matrix('red', 2, 2))
  expect_equivalent(top_padding(ht4), matrix(1, 2, 2))
  expect_equivalent(top_border_style(ht5), matrix('double', 2, 2))
})


test_that('set_outer_borders', {
  ht <- hux(a = 1:3, b = 1:3, c = 1:3)

  check_borders <- function (ht) {
    expect_equivalent(top_border(ht), matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0)), 3, 3)
    expect_equivalent(bottom_border(ht), matrix(c(0, 0, 0, 0, 0, 1, 0, 0, 1)), 3, 3)
    expect_equivalent(left_border(ht), matrix(c(0, 0, 0, 0, 1, 1, 0, 0, 0)), 3, 3)
    expect_equivalent(right_border(ht), matrix(c(0, 0, 0, 0, 0, 0, 0, 1, 1)), 3, 3)
  }
  ht2 <- set_outer_borders(ht, 2:3, 2:3, 1)
  check_borders(ht2)
  ht3 <- set_outer_borders(ht, c(F, T, T), c(F, T, T), 1)
  check_borders(ht3)
  ht4 <- set_outer_borders(ht, 2:3, c("b", "c"), 1)
  check_borders(ht4)
  ht5 <- set_outer_borders(ht, 2:3, tidyselect::matches('b|c'), 1) # testthat has a `matches` function
  check_borders(ht5)
})


test_that('set_outer_borders() works with non-standard/empty position arguments', {
  ht <- hux(a = 1:2, b = 1:2)

  ht2 <- set_outer_borders(ht, 1)
  ht3 <- set_outer_borders(ht, everywhere, everywhere, 1)
  for (h in list(ht2, ht3)) {
    expect_equivalent(top_border(h), matrix(c(1, 0, 1, 0), 2, 2))
    expect_equivalent(bottom_border(h), matrix(c(0, 1, 0, 1), 2, 2))
    expect_equivalent(left_border(h), matrix(c(1, 1, 0, 0), 2, 2))
    expect_equivalent(right_border(h), matrix(c(0, 0, 1, 1), 2, 2))
  }
  ht4 <- set_outer_borders(ht, where(ht > 1.5), 1)
  ht5 <- set_outer_borders(ht, evens, everywhere, 1)
  for (h in list(ht4, ht5)) {
    expect_equivalent(top_border(h), matrix(c(0, 1, 0, 1), 2, 2))
    expect_equivalent(bottom_border(h), matrix(c(0, 1, 0, 1), 2, 2))
    expect_equivalent(left_border(h), matrix(c(0, 1, 0, 0), 2, 2))
    expect_equivalent(right_border(h), matrix(c(0, 0, 0, 1), 2, 2))
  }
})


test_that('merge_cells', {
  ht <- hux(a = 1:3, b = 1:3)
  expect_silent(ht2 <- merge_cells(ht, 1, 1:2))
  expect_silent(ht2 <- merge_cells(ht2, 2:3, 1))
  expect_equivalent(colspan(ht2), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))
  expect_equivalent(rowspan(ht2), matrix(c(1, 2, 1, 1, 1, 1), 3, 2))

  expect_silent(ht3 <- merge_cells(ht, 1, everywhere))
  expect_equivalent(colspan(ht3), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))

  expect_silent(ht4 <- merge_cells(ht, 1:2, 1:2))
  expect_equivalent(colspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))
  expect_equivalent(rowspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))

  expect_silent(ht5 <- merge_cells(ht, c(1, 3), 1))
  expect_equivalent(rowspan(ht5), matrix(c(3, 1, 1, 1, 1, 1), 3, 2))

})


test_that('where', {
  dfr <- data.frame(a = 1:3, b = letters[1:3], d = 3:1, stringsAsFactors = FALSE)
  expect_equivalent(where(dfr == 3), matrix(c(3, 1, 1, 3), 2, 2))
})


test_that('where() works with set_*', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht <- set_font(ht, where(ht > 0), 'times')
  expect_equivalent(font(ht), matrix(c('times', NA, NA, 'times'), 2, 2))
})


test_that('is_a_number', {
  expect_false(is_a_number('foo'))
  expect_true(is_a_number(1.5))
  expect_true(is_a_number('1.5'))
  ht <- hux(a = 1:2, add_colnames = TRUE)
  expect_equivalent(is_a_number(ht), matrix(c(FALSE, TRUE, TRUE), 3, 1))
})
