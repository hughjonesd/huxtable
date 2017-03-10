
context('Row and column functions')


source('functions.R')


test_that('Row/column function examples unchanged', {
  test_ex_same('every')
  test_ex_same('last')
})

test_that('last() works as expected', {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5 , e = 1:5)
  expect_equivalent(last(2)(dfr, 1), 4:5)
  expect_equivalent(last(2)(dfr, 2), 3:4)
  expect_equivalent(last(6)(dfr, 1), 1:5)
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
  ht <- set_font(ht, , evens(), 'times')
  ht <- set_font(ht, , odds(), 'palatino')
  expect_equivalent(font(ht), matrix(c('palatino', 'times'), 4, 2, byrow = TRUE))
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, every(3, from = 1), ,'times')
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
  ht <- set_font(ht, , b:d, 'times')
  expect_equivalent(font(ht), matrix(c(NA, 'times', 'times', 'times'), 4 , 4, byrow = TRUE))
})

test_that('set_properties works with expressions in ht context', {
  ht <- hux(a = 1:4, b = 1:4)
  ht <- set_font(ht, a >= 2 & b <= 3, 1:2, 'times')
  expect_equivalent(font(ht), matrix(c(NA, 'times', 'times', NA), 4 , 2))
})

test_that('set_properties works with expressions in ht context', {
  ht <- hux(a = 1:2, b = 1:2)
  ht <- set_font(ht, 1:2, 1:2, c('times', 'palatino'), byrow = TRUE)
  expect_equivalent(font(ht), matrix(c('times', 'times', 'palatino', 'palatino'), 2, 2))
  ht <- hux(a = 1:2, b = 1:2, d = 1:2)
  ht <- set_font(ht, 2, 1:2, c('times', 'palatino'), byrow = TRUE)
  expect_equivalent(font(ht), matrix(c(NA, 'times', NA, 'palatino', NA, NA), 2, 3))
})

test_that('where() works as expected', {
  dfr <- data.frame(a = 1:3, b = letters[1:3], d = 3:1, stringsAsFactors = FALSE)
  expect_equivalent(where(dfr == 3), matrix(c(3, 1, 1, 3), 2, 2))
})

test_that('is_a_number() works as expected', {
  expect_false(is_a_number('foo'))
  expect_true(is_a_number(1.5))
  expect_true(is_a_number('1.5'))
  ht <- hux(a = 1:2, add_colnames = TRUE)
  expect_equivalent(is_a_number(ht), matrix(c(FALSE, TRUE, TRUE), 3, 1))
})
