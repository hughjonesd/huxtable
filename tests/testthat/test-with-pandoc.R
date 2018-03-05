
context('Test output using pandoc')


source('functions.R')


test_that('Four spaces does not cause <pre><code> markup', {
  skip_without_pandoc()
  skip_on_cran()
  on.exit(if (exists('output')) file.remove(output))
  output <- rmarkdown::render('fourspace-html-test.Rmd', quiet = TRUE)
  lines <- readLines(output)
  expect_false(any(grepl('findme&lt;/td&gt;', lines)))
})


test_that('Row heights do not screw up LaTeX multicol', {
  skip_without_pandoc()
  skip_on_cran()
  skip_on_travis() # temporary
  on.exit(if (exists('output')) file.remove(output))
  expect_silent(output <- rmarkdown::render('rowheight-multicol-test.Rmd', quiet = TRUE))
})


test_that('table-tester-2.Rmd renders without errors in LaTeX', {
  skip_without_pandoc()
  skip_on_cran()
  skip_on_travis() # temporary
  on.exit(if (exists('output')) file.remove(output))
  expect_silent(output <- rmarkdown::render('table-tester-2.Rmd', quiet = TRUE, output_format = "pdf_document"))
})


test_that('table-tester-2.Rmd renders without errors in HTML', {
  skip_without_pandoc()
  skip_on_cran()
  on.exit(if (exists('output')) file.remove(output))
  expect_silent(output <- rmarkdown::render('table-tester-2.Rmd', quiet = TRUE, output_format = "html_document"))
})
