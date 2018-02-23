
context("Output")


source('functions.R')


validate_markdown <- function(md_string, output_format = 'html_document') {
  force(output_format)
  on.exit({
    if (exists('tf')) file.remove(tf)
    if (exists('ht')) file.remove(ht)
  })
  td <- tempdir()
  tf <- tempfile(pattern = 'markdown-example', fileext = '.md', tmpdir = td)
  cat(md_string, file = tf)
  expect_silent(ht <- rmarkdown::render(tf, output_format = output_format, output_file = NULL, output_dir = td,
    intermediates_dir = td, clean = TRUE, quiet = TRUE)) # no error
}


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


test_that('to_md produces valid markdown', {
  skip_without_pandoc()
  ht <- hux(a = 1:5, b = 1:5)
  md <- to_md(ht)
  validate_markdown(md)
  ht <- set_all_borders(ht, 1)
  md <- to_md(ht)
  validate_markdown(md)
})


test_that('to_md and to_screen keep to max_width', {
  ht <- hux(a = paste(sample(LETTERS), collapse = '...'), b = 1:2)

  for (mw in 2:12 * 10) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, max_width = mw)
      lines <- strsplit(output, '\n', fixed = TRUE)[[1]]
      expect_true(all(nchar(lines, type = 'width') <= mw))
    }
  }

  caption(ht) <- 'a very very very long caption'
  output <- to_screen(ht, max_width = 15)
  lines <- strsplit(output, '\n', fixed = TRUE)[[1]]
  expect_true(all(nchar(lines, type = 'width') <= 15))
  # we don't test to_md for captions for to_md because I don't think markdown can handle multiline captions
})


test_that('to_md and to_screen keep to min_width', {
  ht <- hux(a = 'foo', b = 'bar')
  for (mw in c(2, 10, 20)) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, min_width = mw)
      lines <- strsplit(output, '\n', fixed = TRUE)[[1]]
      lines <- lines[nchar(lines) > 0] # empty lines after table itself
      lines <- lines[ ! grepl('Column names', lines)]
      expect_true(all(nchar(lines, type = 'width') >= mw ))
    }
  }
})

test_that('hux_logo works', {
  expect_silent(hux_logo())
  expect_silent(hux_logo(latex = TRUE))
})


test_that('Multi-rowspan screen output is sane', {
  ht <- hux(a = rep('aaaaaa', 10), b = rep('bbbbbb', 10))
  rowspan(ht)[1, 1] <- 10
  expect_equal_to_reference(to_screen(ht), 'multirow.rds')
})


test_that('to_screen does not cut off multicols', {
  ht <- hux(a = 1:2, b = 1:2)
  ht[2, 1] <- 'some very long long text'
  colspan(ht)[2, 1] <- 2
  expect_match(to_screen(ht), 'some very long long text', fixed = TRUE)
})


test_that('guess_knitr_output_format() gets it right', {
  skip_without_pandoc()
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


test_that('Quick output functions work', {
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)

  tf <- tempfile(fileext = '.htm')
  expect_silent(quick_html(m, dfr, ht, file = tf))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = '.docx')
  expect_silent(quick_docx(m, dfr, ht, file = tf))
  expect_true(file.exists(tf))

  tf <- tempfile(fileext = '.xlsx')
  expect_silent(quick_xlsx(m, dfr, ht, file = tf))
  expect_true(file.exists(tf))
})

test_that('quick_pdf works', {
  skip_on_appveyor()
  ht <- hux(a = 1:2, b = 1:2)
  m <- matrix(1:4, 2, 2)
  dfr <- data.frame(a = 1:5, b = 1:5)
  tf <- tempfile(fileext = '.pdf')
  expect_silent(quick_pdf(m, dfr, ht, file = tf))
  expect_true(file.exists(tf))
})


test_that('Quick output functions stop if called non-interactively with no `file` argument', {
  ht <- hux(a = 1:2, b = 1:2)
  expect_error(quick_html(ht))
  expect_false(file.exists('huxtable-output.html'))
})
