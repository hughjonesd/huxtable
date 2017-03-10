
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
  skip_without_pandoc()
  on.exit(if (exists('output')) file.remove(output))
  output <- rmarkdown::render('fourspace-html-test.Rmd', quiet = TRUE)
  lines <- readLines(output)
  expect_false(any(grepl('findme&lt;/td&gt;', lines)))
})

test_that('Row heights do not screw up latex multicol', {
  skip_without_pandoc()
  on.exit(if (exists('output')) file.remove(output))
  expect_silent(output <- rmarkdown::render('rowheight-multicol-test.Rmd', quiet = TRUE))

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

