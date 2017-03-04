
context("Attributes")

source(devtools::package_file('tests', 'testing-functions.R'))

ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)


test_that("Cell property getter/setters work", {
  for (attr in huxtable_cell_attrs) {
    test_ex_same(attr)
  }
})

test_that("Row/col getter/setters work", {
  for (attr in c(huxtable_col_attrs, huxtable_row_attrs)) {
    test_ex_same(attr)
  }
})

test_that("Table property getter/setters work", {
  for (attr in huxtable_table_attrs) {
    test_ex_same(attr)
  }
})
