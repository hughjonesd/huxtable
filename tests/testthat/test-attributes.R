
context("Attributes")

source(devtools::package_file('tests', 'testing-functions.R'))

ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)


test_that("Cell property getter/setter examples unchanged", {
  for (attr in huxtable_cell_attrs) {
    test_ex_same(attr)
  }
})

test_that("Row/col getter/setter examples unchanged", {
  for (attr in c(huxtable_col_attrs, huxtable_row_attrs)) {
    test_ex_same(attr)
  }
})

test_that("Table property getter/setter examples unchanged", {
  for (attr in huxtable_table_attrs) {
    test_ex_same(attr)
  }
})

test_that("Can refer to properties by colnames", {
  number_format(ht)[1,1] <- 3
  col_width(ht) <- c(.2, .6, .2)
  row_height(ht) <- rep(.2, 5)
  expect_equal(number_format(ht)[1, 'a'], list(3))
  expect_equivalent(col_width(ht)['a'], .2)
})

test_that('Assignment to attributes preserves colnames', {
  tmp <- colnames(align(ht))
  align(ht) <- 'right'
  expect_equal(tmp, colnames(align(ht)))
  align(ht)[1,1] <- 'left'
  expect_equal(tmp, colnames(align(ht)))
})
