
context("Printing to various formats")


test_that("LaTeX output examples unchanged", {
  test_ex_same("to_latex")
})


test_that("Markdown output examples unchanged", {
  test_ex_same("to_md")
})


test_that("HTML output examples unchanged", {
  test_ex_same("to_html")
})


test_that("Screen output examples unchanged", {
  test_ex_same("to_screen")
})


test_that("to_screen gives warning with colour if crayon not installed", {
  ht <- hux(a = 1:2)
  with_mock(requireNamespace = function (...) FALSE, {
    expect_warning(to_screen(ht, color = TRUE), "crayon")
  })
})


test_that("to_md and to_screen keep to max_width", {
  ht <- hux(a = paste(sample(LETTERS), collapse = "..."), b = 1:2)

  for (mw in 2:12 * 10) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, max_width = mw)
      lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
      expect_true(all(nchar(lines, type = "width") <= mw))
    }
  }

  caption(ht) <- "a very very very long caption"
  output <- to_screen(ht, max_width = 18)
  lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
  expect_true(all(nchar(lines, type = "width") <= 18))
  # we don't test captions for to_md because I don't think markdown can handle multiline captions
})


test_that("to_md and to_screen keep to min_width", {
  ht <- hux(a = "foo", b = "bar")
  for (mw in c(2, 10, 20)) {
    for (func in list(to_md, to_screen)) {
      output <- func(ht, min_width = mw)
      lines <- strsplit(output, "\n", fixed = TRUE)[[1]]
      lines <- lines[nchar(lines) > 0] # empty lines after table itself
      lines <- lines[ ! grepl("Column names", lines)]
      expect_true(all(nchar(lines, type = "width") >= mw ))
    }
  }
})


test_that("to_md warns on unimplemented features", {
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


test_that("to_md prints bold and italic", {
  short_strings <- c("bold", "both", "italic")
  long_strings <- strrep(toupper(short_strings), 40)
  ht <- hux(a = short_strings, b = long_strings)
  bold(ht)[1:2, 1:2] <- TRUE
  italic(ht)[2:3, 1:2] <- TRUE
  expect_silent(res <- to_md(ht))
  expect_match(res, regexp = "\\*italic\\*")
  expect_match(res, regexp = "\\*\\*bold\\*\\*")
  expect_match(res, regexp = "\\*\\*\\*both\\*\\*\\*")

  res <- strsplit(res, "\\n")[[1]]

  # any strings with ITALIC should have * at start and end
  italic <- stringr::str_match(res, "(.)[ITALIC]{0,6}(ITALIC)+[ITALIC]{0,6}(.)")
  italic <- stats::na.omit(italic)
  expect_true(all(italic[, 2] == "*"))
  expect_true(all(italic[, 4] == "*"))

  # any strings with BOTH should have *** at start and end
  both <- stringr::str_match(res, "(...)[BOTH]{0,4}(BOTH)+[BOTH]{0,4}(...)")
  both <- stats::na.omit(both)
  expect_true(all(both[, 2] == "***"))
  expect_true(all(both[, 4] == "***"))

  # any strings with BOLD should have ** at start and end
  bold <- stringr::str_match(res, "(..)[BOLD]{0,4}(BOLD)+[BOLD]{0,4}(..)")
  bold <- stats::na.omit(bold)
  expect_true(all(bold[, 2] == "**"))
  expect_true(all(bold[, 4] == "**"))
})

test_that("hux_logo works", {
  # there"s randomization, so:
  for (i in 1:100) expect_silent(hux_logo())
  expect_silent(hux_logo(latex = TRUE))
})


test_that("Multi-rowspan screen output is sane", {
  ht <- hux(a = rep("aaaaaa", 10), b = rep("bbbbbb", 10))
  rowspan(ht)[1, 1] <- 10
  expect_equal_to_reference(to_screen(ht), "multirow.rds")
})


test_that("to_screen does not cut off multicols", {
  ht <- hux(a = 1:2, b = 1:2)
  ht[2, 1] <- "some very long long text"
  colspan(ht)[2, 1] <- 2
  expect_match(to_screen(ht), "some very long long text", fixed = TRUE)
})


test_that("output works with zero-dimension huxtables", {
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

test_that("output works with 1x1 huxtables", {
  h_1x1 <- hux(a = 1, add_colnames = FALSE)

  expect_silent(to_screen(h_1x1))
  expect_silent(to_md(h_1x1))
  expect_silent(to_html(h_1x1))
  expect_silent(to_latex(h_1x1))
})


test_that("format.huxtable works", {
  ht <- hux(a = 1:3, b = 1:3)
  for (output in c("latex", "html", "md", "screen")) {
    direct_call <- paste0("to_", output)
    expect_identical(do.call(direct_call, list(ht)), format(ht, output = output))
  }
})


test_that("set_print_method() works", {
  ht <- hux(a = 1:2, b = 1:2)
  oo <- options(huxtable.print = print_html)
  expect_match(capture.output(print(ht)), "<table", fixed = TRUE, all = FALSE)
  options(huxtable.print = print_latex)
  expect_match(capture.output(print(ht)), "tabular", fixed = TRUE, all = FALSE)
  options(huxtable.print = "print_html")
  expect_match(capture.output(print(ht)), "<table", fixed = TRUE, all = FALSE)
  options(oo)
})


test_that("HTML gives warnings for double borders not wide enough", {
  ht <- hux(a = 1)
  top_border(ht) <- 2
  top_border_style(ht) <- "double"
  expect_warning(to_html(ht), "double")
  top_border(ht) <- 3
  expect_silent(to_html(ht))
})


test_that("Chinese characters are not repeated", {
  skip_on_appveyor()
  ht <- hux("\u4e2d\u6587")
  expect_silent(s <- to_screen(ht))
  expect_match(s, "^\\s*\u4e2d\u6587\\s*$", all = FALSE, perl = TRUE)
})
