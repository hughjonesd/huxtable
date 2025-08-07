local_edition(2)

test_that("builders reproduce to_html output", {
  ht <- hux(a = 1:2, b = 3:4)
  expect_identical(
    to_html(ht),
    paste0(
      huxtable:::build_table_style(ht),
      huxtable:::build_colgroup(ht),
      huxtable:::build_row_html(ht, huxtable:::build_cell_html(ht)),
      "</table>\n"
    )
  )
})

test_that("build_cell_html returns correct dimensions", {
  ht <- hux(a = 1:2, b = 3:4)
  cells <- huxtable:::build_cell_html(ht)
  expect_equal(dim(cells), dim(ht))
})
