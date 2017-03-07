
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

test_that('Multi-rowspan screen output is sane', {
  ht <- hux(a = rep('aaaaaa', 10), b = rep('bbbbbb', 10))
  rowspan(ht)[1,1] <- 10
  expect_equal_to_reference(to_screen(ht), 'multirow.rds')
})

test_that('Four spaces does not cause <pre><code> markup', {
  #skip('Waiting for knitr fix')
  output <- rmarkdown::render('fourspace-html-test.Rmd', quiet = TRUE)
  lines <- readLines(output)
  file.remove(output)
  expect_false(any(grepl('findme&lt;/td&gt;', lines)))
})
