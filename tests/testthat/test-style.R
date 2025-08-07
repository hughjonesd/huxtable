local_edition(2)


test_that("Can use style_cells", {
  htxx <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  ht2 <- style_cells(htxx, 1, 1, font = "times", font_size = 24)
  expect_equivalent(font(ht2)[1, 1], "times")
  expect_equivalent(font_size(ht2)[1, 1], 24)
  r <- 2
  c <- 1
  ht3 <- set_cell_properties(htxx, r, c, bold = TRUE, font = "times")
  expect_equivalent(bold(ht3)[2, 1], TRUE)
  expect_equivalent(font(ht3)[2, 1], "times")
})

test_that("Bugfix: style_cells works when huxtable not attached", {
  on.exit(library(huxtable))
  detach("package:huxtable")
  h <- huxtable::huxtable(1)
  expect_silent(
    huxtable::style_cells(h, bold = TRUE)
  )
})


test_that("style_cells fails with bad arguments", {
  ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)
  expect_error(ht <- style_cells(ht, 1, 1, bad = "no!"))
})


test_that("style_cells works with border properties", {
  ht <- huxtable(1:2, 1:2)
  expect_silent(ht <- style_cells(ht, bottom_border = 0.4))
  expect_equivalent(brdr_thickness(bottom_border(ht)), matrix(0.4, 2, 2))

  expect_silent(ht <- style_cells(ht, bottom_border = brdr(0.6)))
  expect_equivalent(brdr_thickness(bottom_border(ht)), matrix(0.6, 2, 2))

  expect_silent(ht <- style_cells(ht, top_border_color = "red"))
  expect_equivalent(top_border_color(ht), matrix("red", 2, 2))
})


test_that("style_headers et al.", {
  ht <- hux(a = 1, add_colnames = TRUE)
  ht <- style_headers(ht, bold = TRUE)
  expect_equivalent(bold(ht), matrix(c(TRUE, FALSE), 2, 1))

  ht <- hux(a = 1, add_colnames = TRUE)
  ht <- style_header_rows(ht, bold = TRUE)
  expect_equivalent(bold(ht), matrix(c(TRUE, FALSE), 2, 1))

  ht <- hux("header", "content", add_colnames = FALSE)
  ht <- set_header_cols(ht, 1, TRUE)
  ht <- style_header_cols(ht, bold = TRUE)
  expect_equivalent(bold(ht), matrix(c(TRUE, FALSE), 1, 2))
})
