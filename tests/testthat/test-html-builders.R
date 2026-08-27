local_edition(3)

test_that("builders reproduce to_html output", {
  ht <- hux(a = 1:2, b = 3:4)
  expect_identical(
    to_html(ht, dependencies = FALSE),
    paste0(
      huxtable:::build_table_style(ht),
      huxtable:::build_colgroup(ht),
      huxtable:::build_row_html(ht, huxtable:::build_cell_html(ht)),
      "</table>\n"
    )
  )
})


test_that("HTML dependencies can be included or omitted", {
  ht <- hux(a = 1)
  expect_match(to_html(ht), "<style>", fixed = TRUE)
  expect_false(grepl("<style>", to_html(ht, dependencies = FALSE), fixed = TRUE))
  expect_true(any(grepl("<style>", capture.output(print_html(ht)), fixed = TRUE)))
  expect_false(any(grepl(
    "<style>", capture.output(print_html(ht, dependencies = FALSE)), fixed = TRUE
  )))
})


test_that("build_cell_html returns correct dimensions", {
  ht <- hux(a = 1:2, b = 3:4)
  cells <- huxtable:::build_cell_html(ht)
  expect_equal(dim(cells), dim(ht))
})


test_that("HTML applies table and row break rules", {
  ht <- hux(a = 1:2)
  html <- to_html(ht)
  expect_match(html, "break-inside: avoid; page-break-inside: avoid;", fixed = TRUE)

  breakable(ht) <- TRUE
  html <- to_html(ht)
  expect_match(html, "break-inside: auto; page-break-inside: auto;", fixed = TRUE)
  expect_match(
    huxtable_html_css(),
    ".huxtable tr {\n  break-inside: avoid;\n  page-break-inside: avoid;",
    fixed = TRUE
  )
})


test_that("HTML applies a background to the table element", {
  ht <- set_table_background_color(hux(a = 1), "red")
  html <- to_html(ht)
  expect_match(html, "background-color: rgb(255, 0, 0);", fixed = TRUE)
  expect_false(any(grepl("background-color", huxtable:::build_cell_html(ht), fixed = TRUE)))

  background_color(ht)[1, 1] <- "blue"
  expect_match(
    huxtable:::build_cell_html(ht)[1, 1],
    "background-color: rgb(0, 0, 255);",
    fixed = TRUE
  )
})
