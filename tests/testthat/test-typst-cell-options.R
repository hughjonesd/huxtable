local_edition(2)

# Tests for typst_cell_options

test_that("padding translates to typst inset", {
  ht <- hux(a = 1)
  ht <- set_left_padding(ht, 1)
  ht <- set_right_padding(ht, 2)
  ht <- set_top_padding(ht, 3)
  ht <- set_bottom_padding(ht, 4)
  opts <- huxtable:::typst_cell_options(ht)
  expect_identical(
    opts[1, 1],
    "inset: (top: 3pt, right: 2pt, bottom: 4pt, left: 1pt)"
  )
})
