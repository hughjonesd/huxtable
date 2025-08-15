local_edition(3)


test_that("set_all_*", {
  ht <- hux(a = c(1, 0), b = c(0, 1), add_colnames = FALSE)
  ht2 <- set_all_borders(ht, 1)
  expect_equal(brdr_thickness(top_border(ht2)), matrix(1, 2, 2), ignore_attr = TRUE)

  ht4 <- set_all_borders(ht, 1, 2, 1)
  expect_equal(brdr_thickness(top_border(ht4))[[1, 2]], 1)
  expect_equal(brdr_thickness(left_border(ht4))[[1, 2]], 1)
  expect_equal(brdr_thickness(bottom_border(ht4))[[1, 2]], 1)
  expect_equal(brdr_thickness(right_border(ht4))[[1, 2]], 1)
  expect_equal(brdr_thickness(top_border(ht4))[[2, 1]], 0)
  expect_equal(brdr_thickness(left_border(ht4))[[2, 1]], 0)
  expect_equal(brdr_thickness(bottom_border(ht4))[[2, 1]], 0)
  expect_equal(brdr_thickness(right_border(ht4))[[2, 1]], 0)

  rownum <- 1
  colnum <- 2
  ht5 <- set_all_borders(ht, rownum, colnum, 1)
  expect_equal(top_border(ht5), top_border(ht4), ignore_attr = TRUE)
  expect_equal(left_border(ht5), left_border(ht4), ignore_attr = TRUE)
  expect_equal(bottom_border(ht5), bottom_border(ht4), ignore_attr = TRUE)
  expect_equal(right_border(ht5), right_border(ht4), ignore_attr = TRUE)

  border_size <- 2
  ht6 <- set_all_borders(ht, border_size)
  expect_equal(brdr_thickness(top_border(ht6)), matrix(border_size, 2, 2), ignore_attr = TRUE)

  ht7 <- set_all_borders(ht, 1:2, tidyselect::matches("a|b"), 1)
  expect_equal(brdr_thickness(top_border(ht7)), matrix(1, 2, 2), ignore_attr = TRUE)
})


test_that("set_lr/tb_* functions", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)
  expect_equal(brdr_thickness(left_border(set_lr_borders(ht))), matrix(0.4, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(right_border(set_lr_borders(ht))), matrix(0.4, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(top_border(set_lr_borders(ht))), matrix(0, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(bottom_border(set_lr_borders(ht))), matrix(0, 2, 2), ignore_attr = TRUE)

  expect_equal(brdr_thickness(left_border(set_tb_borders(ht))), matrix(0, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(right_border(set_tb_borders(ht))), matrix(0, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(top_border(set_tb_borders(ht))), matrix(0.4, 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(bottom_border(set_tb_borders(ht))), matrix(0.4, 2, 2), ignore_attr = TRUE)

  expect_equal(left_border_color(set_lr_border_colors(ht, "red")), matrix("red", 2, 2), ignore_attr = TRUE)
  expect_equal(right_border_color(set_lr_border_colors(ht, "red")), matrix("red", 2, 2), ignore_attr = TRUE)
  expect_equal(top_border_color(set_lr_border_colors(ht, "red")), matrix(NA_character_, 2, 2), ignore_attr = TRUE)
  expect_equal(
    bottom_border_color(set_lr_border_colors(ht, "red")),
    matrix(NA_character_, 2, 2),
    ignore_attr = TRUE
  )

  expect_equal(
    left_border_color(set_tb_border_colors(ht, "red")),
    matrix(NA_character_, 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    right_border_color(set_tb_border_colors(ht, "red")),
    matrix(NA_character_, 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    top_border_color(set_tb_border_colors(ht, "red")),
    matrix("red", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    bottom_border_color(set_tb_border_colors(ht, "red")),
    matrix("red", 2, 2),
    ignore_attr = TRUE
  )

  expect_equal(
    left_border_style(set_lr_border_styles(ht, "double")),
    matrix("double", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    right_border_style(set_lr_border_styles(ht, "double")),
    matrix("double", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    top_border_style(set_lr_border_styles(ht, "double")),
    matrix("solid", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    bottom_border_style(set_lr_border_styles(ht, "double")),
    matrix("solid", 2, 2),
    ignore_attr = TRUE
  )

  expect_equal(
    left_border_style(set_tb_border_styles(ht, "double")),
    matrix("solid", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    right_border_style(set_tb_border_styles(ht, "double")),
    matrix("solid", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    top_border_style(set_tb_border_styles(ht, "double")),
    matrix("double", 2, 2),
    ignore_attr = TRUE
  )
  expect_equal(
    bottom_border_style(set_tb_border_styles(ht, "double")),
    matrix("double", 2, 2),
    ignore_attr = TRUE
  )
})


test_that("set_all_* functions work when huxtable is not attached", {
  # NB as written this test can only be run from the command line; detach call silently fails
  library(huxtable)
  detach(package:huxtable)
  ht <- huxtable::hux(a = c(1, 0), b = c(0, 1), add_colnames = FALSE)
  expect_silent(ht2 <- huxtable::set_all_borders(ht, 1))
  expect_silent(ht3 <- huxtable::set_all_border_colors(ht, "red"))
  expect_silent(ht4 <- huxtable::set_all_padding(ht, 1))
  expect_silent(ht5 <- huxtable::set_all_border_styles(ht, "double"))
  library(huxtable) # we reattach before these tests, or we have problems with unavailable methods
  expect_equal(brdr_thickness(top_border(ht2)), matrix(1, 2, 2), ignore_attr = TRUE)
  expect_equal(top_border_color(ht3), matrix("red", 2, 2), ignore_attr = TRUE)
  expect_equal(top_padding(ht4), matrix(1, 2, 2), ignore_attr = TRUE)
  expect_equal(top_border_style(ht5), matrix("double", 2, 2), ignore_attr = TRUE)
})


test_that("set_outer_*", {
  ht <- hux(a = 1:3, b = 1:3, c = 1:3, add_colnames = FALSE)

  check_borders <- function(ht, suffix, un, set) {
    wrapper <- if (suffix == "") brdr_thickness else identity
    funcs <- paste0(c("top", "bottom", "left", "right"), sprintf("_border%s", suffix))
    funcs <- mget(funcs, inherits = TRUE)
    expect_equal(
      wrapper(funcs[[1]](ht)),
      matrix(c(un, un, un, un, set, un, un, set, un), 3, 3),
      ignore_attr = TRUE
    )
    expect_equal(
      wrapper(funcs[[2]](ht)),
      matrix(c(un, un, un, set, un, set, set, un, set), 3, 3),
      ignore_attr = TRUE
    )
    expect_equal(
      wrapper(funcs[[3]](ht)),
      matrix(c(un, un, un, un, set, set, un, un, un), 3, 3),
      ignore_attr = TRUE
    )
    expect_equal(
      wrapper(funcs[[4]](ht)),
      matrix(c(un, set, set, un, un, un, un, set, set), 3, 3),
      ignore_attr = TRUE
    )
  }

  ht2 <- set_outer_borders(ht, 2:3, 2:3, 1)
  check_borders(ht2, "", 0, 1)
  ht3 <- set_outer_borders(ht, c(F, T, T), c(F, T, T), 1)
  check_borders(ht3, "", 0, 1)
  ht4 <- set_outer_borders(ht, 2:3, c("b", "c"), 1)
  check_borders(ht4, "", 0, 1)
  # NB: testthat has a `matches` function
  ht5 <- set_outer_borders(ht, 2:3, tidyselect::matches("b|c"), 1)
  check_borders(ht5, "", 0, 1)

  ht2 <- set_outer_border_colors(ht, 2:3, 2:3, "red")
  check_borders(ht2, "_color", NA, "red")
  ht3 <- set_outer_border_colors(ht, c(F, T, T), c(F, T, T), "red")
  check_borders(ht3, "_color", NA, "red")
  ht4 <- set_outer_border_colors(ht, 2:3, c("b", "c"), "red")
  check_borders(ht4, "_color", NA, "red")
  # NB: testthat has a `matches` function
  ht5 <- set_outer_border_colors(ht, 2:3, tidyselect::matches("b|c"), "red")
  check_borders(ht5, "_color", NA, "red")

  ht2 <- set_outer_border_styles(ht, 2:3, 2:3, "double")
  check_borders(ht2, "_style", "solid", "double")
  ht3 <- set_outer_border_styles(ht, c(F, T, T), c(F, T, T), "double")
  check_borders(ht3, "_style", "solid", "double")
  ht4 <- set_outer_border_styles(ht, 2:3, c("b", "c"), "double")
  check_borders(ht4, "_style", "solid", "double")
  # NB: testthat has a `matches` function
  ht5 <- set_outer_border_styles(ht, 2:3, tidyselect::matches("b|c"), "double")
  check_borders(ht5, "_style", "solid", "double")
})


test_that("set_outer_borders() works with non-standard/empty position arguments", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)

  ht2 <- set_outer_borders(ht, 1)
  ht3 <- set_outer_borders(ht, everywhere, everywhere, 1)
  for (h in list(ht2, ht3)) {
    expect_equal(brdr_thickness(top_border(h)), matrix(c(1, 0, 1, 0), 2, 2), ignore_attr = TRUE)
    expect_equal(brdr_thickness(bottom_border(h)), matrix(c(0, 1, 0, 1), 2, 2), ignore_attr = TRUE)
    expect_equal(brdr_thickness(left_border(h)), matrix(c(1, 1, 0, 0), 2, 2), ignore_attr = TRUE)
    expect_equal(brdr_thickness(right_border(h)), matrix(c(0, 0, 1, 1), 2, 2), ignore_attr = TRUE)
  }

  ht4 <- set_outer_borders(ht, evens, everywhere, 1)
  expect_equal(brdr_thickness(top_border(ht4)), matrix(c(0, 1, 0, 1), 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(bottom_border(ht4)), matrix(c(1, 1, 1, 1), 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(left_border(ht4)), matrix(c(0, 1, 0, 0), 2, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(right_border(ht4)), matrix(c(0, 0, 0, 1), 2, 2), ignore_attr = TRUE)
})


test_that("set_outer_padding", {
  ht <- hux(1:2, 1:2)
  ht2 <- set_outer_padding(ht, 10)
  expect_equal(left_padding(ht2), matrix(c(10, 10, 6, 6), 2, 2), ignore_attr = TRUE)
  expect_equal(top_padding(ht2), matrix(c(10, 6, 10, 6), 2, 2), ignore_attr = TRUE)
  expect_equal(bottom_padding(ht2), matrix(c(6, 10, 6, 10), 2, 2), ignore_attr = TRUE)
  expect_equal(right_padding(ht2), matrix(c(6, 6, 10, 10), 2, 2), ignore_attr = TRUE)

  ht3 <- set_outer_padding(ht, 2, 2, 10)
  expect_equal(left_padding(ht3), matrix(c(6, 6, 6, 10), 2, 2), ignore_attr = TRUE)
  expect_equal(top_padding(ht3), matrix(c(6, 6, 6, 10), 2, 2), ignore_attr = TRUE)
  expect_equal(bottom_padding(ht3), matrix(c(6, 6, 6, 10), 2, 2), ignore_attr = TRUE)
  expect_equal(right_padding(ht3), matrix(c(6, 6, 6, 10), 2, 2), ignore_attr = TRUE)
})

test_that("set_markdown_contents", {
  ht <- hux(1:2, 1:2)
  expect_silent(ht2 <- set_markdown_contents(ht, "**foo**"))
  expect_equal(markdown(ht2), matrix(TRUE, 2, 2), ignore_attr = TRUE)
  expect_equal(as.matrix(ht2), matrix("**foo**", 2, 2), ignore_attr = TRUE)

  expect_silent(ht2 <- set_markdown_contents(ht, 1, 2, "**foo**"))
  expect_equal(markdown(ht2), matrix(c(FALSE, FALSE, TRUE, FALSE), 2, 2), ignore_attr = TRUE)
  expect_equal(as.character(ht2[[1, 2]]), "**foo**")
})
