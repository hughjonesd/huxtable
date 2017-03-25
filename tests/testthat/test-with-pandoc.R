
context('Test output using pandoc')


source('functions.R')


test_that('Four spaces does not cause <pre><code> markup', {
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
