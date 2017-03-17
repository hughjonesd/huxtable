
context('Row and column functions, set_* interface')


source('functions.R')


test_that('Row/column function examples unchanged', {
  test_ex_same('every')
  test_ex_same('final')
  test_ex_same('where')
})

test_that('final() works as expected', {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5 , e = 1:5)
  expect_equivalent(final(2)(dfr, 1), 4:5)
  expect_equivalent(final(2)(dfr, 2), 3:4)
  expect_equivalent(final(6)(dfr, 1), 1:5)
})


test_that('every(), evens and odds work as expected', {
  dfr <- data.frame(a = 1:7, b = 1:7, d = 1:7 , e = 1:7)
  expect_equivalent(every(3, from = 1)(dfr, 1), c(1, 4, 7))
  expect_equivalent(every(3, from = 1)(dfr, 2), c(1, 4))
  expect_equivalent(evens()(dfr, 1), c(2, 4, 6))
  expect_equivalent(evens()(dfr, 2), c(2, 4))
  expect_equivalent(odds()(dfr, 1), c(1, 3, 5, 7))
  expect_equivalent(odds()(dfr, 2), c(1, 3))
  expect_equivalent(odds(2)(dfr, 1), c(3, 5, 7))
  expect_equivalent(odds(3)(dfr, 1), c(3, 5, 7))
  expect_equivalent(evens(2)(dfr, 1), c(2, 4, 6))
  expect_equivalent(evens(3)(dfr, 1), c(4, 6))

})

test_that('set_properties works with cell functions', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, evens(), 1:2, 'times')
  ht <- set_font(ht, odds(), 1:2, 'palatino')
  expect_equivalent(font(ht), matrix(c('palatino', 'times'), 4, 2))
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, every(1), evens(), 'times')
  ht <- set_font(ht, every(1), odds(), 'palatino')
  expect_equivalent(font(ht), matrix(c('palatino', 'times'), 4, 2, byrow = TRUE))
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, every(3, from = 1), every(1),'times')
  expect_equivalent(font(ht), matrix(c('times', NA, NA, 'times'), 4, 2))
})


test_that('set_properties works with row and column functions', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_col_width(ht, evens(), '20pt')
  ht <- set_col_width(ht, odds(), '40pt')
  ht <- set_row_height(ht, evens(), '15pt')
  ht <- set_row_height(ht, odds(), '30pt')
  expect_equivalent(col_width(ht), c('40pt', '20pt'))
  expect_equivalent(row_height(ht), rep(c('30pt', '15pt'), 2))
})

test_that('set_properties works with column ranges', {
  ht <- hux(a = 1:4, b = 1:4, c = 1:4, d = 1:4)
  ht <- set_font(ht, every(1), b:d, 'times')
  expect_equivalent(font(ht), matrix(c(NA, 'times', 'times', 'times'), 4 , 4, byrow = TRUE))
})

test_that('set_properties works with expressions in ht context', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, a >= 2 & b <= 3, 1:2, 'times')
  expect_equivalent(font(ht), matrix(c(NA, 'times', 'times', NA), 4 , 2))
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


test_that('set_* works when col is missing', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_font(ht, where(ht > 0), value = 'times')
  expect_equivalent(font(ht2), matrix(c('times', NA, NA, 'times'), 2, 2))
  ht3 <- set_font(ht, where(ht > 0), 'times')
  expect_equivalent(font(ht3), matrix(c('times', NA, NA, 'times'), 2, 2))
  expect_error(set_font(ht, 1:3, 'times')) # no cols!
  expect_error(set_font(ht, where(ht > 0), 'times', byrow = TRUE)) # hard to interpret
})

test_that('set_* works when row and col are missing', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_font(ht, 'times')
  expect_equivalent(font(ht2), matrix('times', 2, 2))
  ht3 <- set_font(ht, value = 'times')
  expect_equivalent(font(ht3), matrix('times', 2, 2))
  ht4 <- set_font(ht, value = c('times', 'arial'), byrow = TRUE)
  expect_equivalent(font(ht4), matrix(c('times', 'times', 'arial', 'arial'), 2, 2))
  ht5 <- set_font(ht, c('times', 'arial'), byrow = TRUE)
  expect_equivalent(font(ht5), matrix(c('times', 'times', 'arial', 'arial'), 2, 2))
})


test_that('all forms of set_all_* work as expected', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht2 <- set_all_borders(ht, 1)
  expect_equivalent(top_border(ht2), matrix(1, 2, 2))
  ht3 <- set_all_borders(ht, where(ht > 0), 1)
  expect_equivalent(top_border(ht3), matrix(c(1, 0, 0, 1), 2, 2))
  ht4 <- set_all_borders(ht, 1, 2, 1)
  expect_equivalent(top_border(ht4), matrix(c(0, 0, 1, 0), 2, 2))
  ht5 <- set_all_borders(ht, 1, a:b, 1)
  expect_equivalent(top_border(ht5), matrix(c(1, 0, 1, 0), 2, 2))
  ht6 <- set_all_borders(ht, a == 0, a:b, 1)
  expect_equivalent(top_border(ht6), matrix(c(0, 1, 0, 1), 2, 2))
})



test_that('where() works as expected', {
  dfr <- data.frame(a = 1:3, b = letters[1:3], d = 3:1, stringsAsFactors = FALSE)
  expect_equivalent(where(dfr == 3), matrix(c(3, 1, 1, 3), 2, 2))
})


test_that('where() works with set_*', {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht <- set_font(ht, where(ht > 0), 'times')
  expect_equivalent(font(ht), matrix(c('times', NA, NA, 'times'), 2, 2))
})


test_that('is_a_number() works as expected', {
  expect_false(is_a_number('foo'))
  expect_true(is_a_number(1.5))
  expect_true(is_a_number('1.5'))
  ht <- hux(a = 1:2, add_colnames = TRUE)
  expect_equivalent(is_a_number(ht), matrix(c(FALSE, TRUE, TRUE), 3, 1))
})
