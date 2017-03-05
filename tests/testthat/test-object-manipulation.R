
context("Object manipulation")

source(devtools::package_file('tests', 'testing-functions.R'))

ht <- huxtable(a = 1:3, b = 1:3)

test_that('Object creation works', {
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




