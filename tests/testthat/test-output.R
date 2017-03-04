
context("Output")

source(devtools::package_file('tests', 'testing-functions.R'))

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
