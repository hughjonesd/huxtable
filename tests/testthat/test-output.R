
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
  ht <- set_caption(ht, "Some caption")
  md <- to_md(ht)
  validate_markdown(md)
  expect_match(md, "Some caption", fixed = TRUE)
})


test_that('to_screen gives warning with colour if crayon not installed', {
  ht <- hux(a = 1:2)
  with_mock(requireNamespace = function (...) FALSE, {
    expect_warning(to_screen(ht, color = TRUE), "crayon")
  })
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
  # we don't test captions for to_md because I don't think markdown can handle multiline captions
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


test_that('to_md warns on unimplemented features', {
  ht <- hux(a = 1:2, b = 1:2)
  colspan(ht)[1, 1] <- 2
  expect_warning(to_md(ht), "colspan")
  colspan(ht)[1, 1] <- 1
  rowspan(ht)[1, 1] <- 2
  expect_warning(to_md(ht), "rowspan")
  rowspan(ht)[1, 1] <- 1
  align(ht)[1, ] <- c("left", "right")
  expect_warning(to_md(ht), "align")
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


test_that('output works with zero-dimension huxtables', {
  h_nrow0 <- hux(a = character(0), b = character(0), add_colnames = FALSE)
  expect_silent(to_screen(h_nrow0))
  expect_warning(to_md(h_nrow0), "row")
  expect_warning(to_html(h_nrow0), "row")
  expect_warning(to_latex(h_nrow0), "row")

  h_ncol0 <- hux(a = 1:2)[, FALSE]
  expect_silent(to_screen(h_ncol0))
  expect_warning(to_md(h_ncol0), "col")
  expect_warning(to_html(h_ncol0), "col")
  expect_warning(to_latex(h_ncol0), "col")
})


test_that('format.huxtable works', {
  ht <- hux(a = 1:3, b = 1:3)
  for (output in c('latex', 'html', 'md', 'screen')) {
    direct_call <- paste0('to_', output)
    expect_identical(do.call(direct_call, list(ht)), format(ht, output = output))
  }
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
  oo <- options(huxtable.print = print_html)
  expect_match(capture.output(print(ht)), '<table', fixed = TRUE, all = FALSE)
  options(huxtable.print = print_latex)
  expect_match(capture.output(print(ht)), 'tabular', fixed = TRUE, all = FALSE)
  options(huxtable.print = 'print_html')
  expect_match(capture.output(print(ht)), '<table', fixed = TRUE, all = FALSE)
  options(oo)
})


expect_outputs_unchanged <- function (hx, idx) {
  info <- paste0("Index i = ", idx)
  file <- file.path(test_path(), "example-rds", paste0("various-outputs-", idx))
  expect_known_value(to_screen(hx), file = paste0(file, "-screen.rds"), info = info)
  expect_known_value(to_md(hx),     file = paste0(file, "-md.rds"),     info = info)
  expect_known_value(to_html(hx),   file = paste0(file, "-html.rds"),   info = info)
  expect_known_value(to_latex(hx),  file = paste0(file, "-latex.rds"),  info = info)
}


test_that('various outputs unchanged', {
  skip_on_R_CMD_check()
  hx <- hux(
          int  = 1:3,
          real = 1:3 + 0.005,
          char = letters[1:3],
          date = as.Date(1:3, origin = "1970-01-01"),
          fact = factor(letters[4:6])
        )
  variations <- expand.grid(
          align             = c('left', 'right', 'centre', '.'),
          background_color  = c('red', grey(.6), NA),
          bold              = c(TRUE, FALSE),
          escape_contents   = c(TRUE, FALSE),
          font_size         = c(8, 10),
          italic            = c(TRUE, FALSE),
          left_border       = c(0, 1, 2),
          left_border_color = c('red', grey(.6)),
          left_padding      = c(0, 4),
          number_format     = c("%3.1g", NA),
          rotation          = c(0, 90),
          text_color        = c('red', grey(.6)),
          valign            = c('top', 'middle', 'bottom'),
          wrap              = c(TRUE, FALSE),
          stringsAsFactors  = FALSE,
          KEEP.OUT.ATTRS   = FALSE
        )
  RNGversion("3.3.0")
  set.seed(271075L) # expect_unchanged is useless if we always pick new variations
  for (i in sample(nrow(variations), 300)) {
    props <- as.list(variations[i,])
    props$ht <- hx
    hx_set <- do.call("set_cell_properties", props)
    expect_outputs_unchanged(hx_set, i)
  }
})
