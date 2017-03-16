
context("Output")


source('functions.R')


test_that('LaTeX output examples unchanged', {
  test_ex_same('to_latex')
})


test_that('Markdown output examples unchanged', {
  test_ex_same('to_md')
})


test_that('HTML output examples unchanged', {
  test_ex_same('to_html')
})


test_that('Screen output examples unchanged', {
  test_ex_same('to_screen')
})


test_that('FlexTable output examples unchanged', {
  skip('Output too screwed up, perhaps by Java references')
  if (! requireNamespace('ReporteRs')) skip('ReporteRs not installed')
  test_ex_same('as_FlexTable.huxtable')
})


test_that('Multi-rowspan screen output is sane', {
  ht <- hux(a = rep('aaaaaa', 10), b = rep('bbbbbb', 10))
  rowspan(ht)[1,1] <- 10
  expect_equal_to_reference(to_screen(ht), 'multirow.rds')
})

test_that('to_screen does not cut off multicols', {
  ht <- hux(a = 1:2, b = 1:2)
  ht[2,1] <- 'some very long long text'
  colspan(ht)[2, 1] <- 2
  expect_match(to_screen(ht), 'some very long long text', fixed = TRUE)
})

test_that('guess_knitr_output_format() gets it right', {
  out <- character(0)
  on.exit(sapply(out, function (x) if (file.exists(x)) file.remove(x)))
  expect_silent(out[1] <- knitr::knit('guess-output-format-test.Rhtml', quiet = TRUE))
  expect_silent(out[2] <- knitr::knit('guess-output-format-test.Rnw', quiet = TRUE))
  for (fname in paste0('guess-output-format-test-Rmd-', c('html.Rmd', 'pdf.Rmd'))) {
    expect_silent(out[fname] <- rmarkdown::render(fname, quiet = TRUE, run_pandoc = FALSE))
  }
})

test_that('set_print_method() works', {
  ht <- hux(a = 1:2, b = 1:2)
  options(huxtable.print = print_html)
  expect_match(capture.output(print(ht)), '<table', fixed = TRUE, all = FALSE)
  options(huxtable.print = print_latex)
  expect_match(capture.output(print(ht)), 'tabular', fixed = TRUE, all = FALSE)
  options(huxtable.print = 'print_html')
  expect_match(capture.output(print(ht)), '<table', fixed = TRUE, all = FALSE)
})


