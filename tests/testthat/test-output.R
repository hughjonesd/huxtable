
context("Output")

source(devtools::package_file('tests', 'testthat', 'functions.R'))

test_that('LaTeX output works', {
  test_ex_same('to_latex')
})


test_that('Markdown output works', {
  test_ex_same('to_md')
})


test_that('HTML output works', {
  test_ex_same('to_html')
})


test_that('Screen output works', {
  test_ex_same('to_screen')
})

test_that('Multi-rowspan screen output is sane', {
  require('stringi')
  set.seed(27101975)
  ht <- hux(a = stringi::stri_rand_strings(10, 6), b = stringi::stri_rand_strings(10, 6))
  rowspan(ht)[1,1] <- 10
  expect_equal_to_reference(to_screen(ht), 'multirow.rds')
})
