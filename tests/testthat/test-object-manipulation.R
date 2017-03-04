
context("Object manipulation")

source(devtools::package_file('tests', 'testing-functions.R'))

test_that('Object creation works', {
  test_ex_same('huxtable')
})

test_that('Object subsetting and replacement work', {
  test_ex_same('extract-methods')
  test_ex_same('add_colnames')
  test_ex_same('cbind.huxtable')
  test_ex_same('t.huxtable')
})


