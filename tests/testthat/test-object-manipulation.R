
context("Object manipulation")


source('functions.R')


ht <- huxtable(a = 1:3, b = 1:3)

test_that('Object creation examples unchanged', {
  test_ex_same('huxtable')
})

test_that('Object subsetting and replacement examples unchanged', {
  test_ex_same('extract-methods')
  test_ex_same('add_colnames')
  test_ex_same('cbind.huxtable')
  test_ex_same('t.huxtable')
})

test_that('Subsetting preserves rownames', {
  rownames(ht) <- letters[1:3]
  expect_equal(rownames(ht[1:2,]), letters[1:2])
})

test_that('Subsetting cuts colspan', {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  rowspan(ht)[1, 1] <- 3
  colspan(ht)[1, 2] <- 2
  ss <- ht[1:2, 1:2]
  expect_equivalent(rowspan(ss)[1, 1], 2)
  expect_equivalent(colspan(ss)[1, 2], 1)
})

test_that('Spans can\'t be overwritten', {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  colspan(ht)[1, 2] <- 2
  expect_error(colspan(ht)[1, 1] <- 2)
})

test_that('Column names are not uglified', {
  skip('Can\'t solve this one at the moment.')
  ht <- hux('A long column name' = 1:3, 'Another name' = 1:3, add_colnames = TRUE)
  expect_match(to_screen(ht), 'A long column name', fixed = TRUE, all = FALSE)
  ht <- hux('A long column name' = 1:3, 'Another name' = 1:3, add_colnames = TRUE)
  ht <- huxtable::add_colnames(ht)
  expect_match(to_screen(ht), 'A long column name', fixed = TRUE, all = FALSE)
})

test_that('add_colnames works with as_hux for matrices', {
  mat <- matrix(1:4, 2, 2, dimnames = list(letters[1:2], LETTERS[1:2]))
  ht <- as_hux(mat, add_colnames = TRUE, add_rownames = TRUE)
  expect_equivalent(ht[1, 2:3], colnames(mat))
  expect_equivalent(ht$rownames[2:3], rownames(mat))
})




