

context("dplyr functions")
skip_if_not_installed("dplyr")

test_that("select and rename work", {
  ht <- hux(a = 1:2, b = 1:2, c = 1:2, d = 1:2)
  bold(ht)[1, ] <- TRUE
  ht2 <- dplyr::select(ht, b:c)
  expect_equivalent(bold(ht2), matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2))
  expect_identical(ht2, ht[, 2:3])
  ht3 <- dplyr::rename(ht, jim = d, bob = c)
  expect_equivalent(colnames(ht3), c("a", "b", "bob", "jim"))
  expect_equivalent(bold(ht3), bold(ht))
})


test_that("slice, filter, arrange and pull work", {
  ht <- hux(a = 1:4, b = c(1, 3, 4, 2))
  row_height(ht) <- c(.4, .2, .1, .3)

  ht2 <- dplyr::slice(ht, c(4, 2))
  expect_identical(ht2, ht[c(4, 2), ])

  ht3 <- dplyr::arrange(ht, b)
  expect_identical(ht3, ht[c(1, 4, 2, 3), ])

  ht4 <- dplyr::filter(ht, a <= 2, b <= 2)
  expect_identical(ht4, ht[1, ])

  vec <- dplyr::pull(ht, a)
  expect_identical(vec, ht$a)
})


test_that("mutate, mutate_ and transmute work", {
  ht <- hux(a = 1:3, b = 1:3)
  bold(ht)[1, ] <- TRUE

  ht2 <- dplyr::mutate(ht, x = a + b)
  expect_equivalent(ht2$x, c(2, 4, 6))
  expect_equivalent(bold(ht2)[, 3], c(TRUE, FALSE, FALSE))

  ht3 <- dplyr::mutate(ht, x = a + b, copy_cell_props = FALSE)
  expect_equivalent(bold(ht3)[, 3], c(FALSE, FALSE, FALSE))
  expect_identical(names(ht3), c("a", "b", "x"))

  ht4 <- dplyr::transmute(ht, x = a + b, a = a + b)
  expect_equivalent(ht4$x, c(2, 4, 6))
  expect_equivalent(bold(ht4)[, 1], c(FALSE, FALSE, FALSE))
  expect_equivalent(bold(ht4)[, 2], c(TRUE, FALSE, FALSE))

  expect_silent(ht5 <- dplyr::mutate(ht, a = NULL))
  expect_equivalent(dim(font(ht5)), c(3, 1))

  ht6 <- dplyr::mutate_(ht, .dots = list(x = quote(a + b)))
  expect_equivalent(ht6$x, c(2, 4, 6))
  expect_equivalent(bold(ht6)[, 3], c(TRUE, FALSE, FALSE))

  ht7 <- dplyr::mutate_(ht, .dots = list(x = quote(a + b), copy_cell_props = FALSE))
  expect_equivalent(ht7$x, c(2, 4, 6))
  expect_equivalent(bold(ht7)[, 3], c(FALSE, FALSE, FALSE))
  expect_identical(names(ht7), c("a", "b", "x"))
})


test_that("set_* works with magrittr pipe", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  expect_silent(ht2 <- ht_orig %>% set_font("times"))
  expect_silent(ht3 <- ht_orig %>% set_all_borders(1))
  expect_equivalent(font(ht2), matrix("times", 2, 2))
  expect_equivalent(top_border(ht3), matrix(1, 2, 2))
})


test_that("dplyr select helpers can be used in set_* functions", {
  suppressWarnings(library(tidyselect))
  ht <- hux(a = 1:5, big = 1:5, bog = 1:5, bin = 1:5)

  expect_silent(ht2 <- set_bold(ht, everywhere, starts_with("b"), TRUE))
  expect_equivalent(bold(ht2)[, -1], matrix(TRUE, 5, 3))
  expect_equivalent(bold(ht2)[, 1], rep(FALSE, 5))

  expect_silent(ht3 <- set_bold(ht, everywhere, ends_with("g"), TRUE))
  expect_equivalent(bold(ht3)[, 2:3], matrix(TRUE, 5, 2))
  expect_equivalent(bold(ht3)[, c(1, 4)], matrix(FALSE, 5, 2))

  expect_silent(ht4 <- set_bold(ht, everywhere, contains("i"), TRUE))
  expect_equivalent(bold(ht4)[, c(2, 4)], matrix(TRUE, 5, 2))
  expect_equivalent(bold(ht4)[, c(1, 3)], matrix(FALSE, 5, 2))

  expect_silent(ht5 <- set_bold(ht, everywhere, matches("b[^o]"), TRUE))
  expect_equivalent(bold(ht5)[, c(2, 4)], matrix(TRUE, 5, 2))
  expect_equivalent(bold(ht5)[, c(1, 3)], matrix(FALSE, 5, 2))

  col_names <- c("big", "bog")
  expect_silent(ht6 <- set_bold(ht, everywhere, one_of(col_names), TRUE))
  expect_equivalent(bold(ht6)[, 2:3], matrix(TRUE, 5, 2))
  expect_equivalent(bold(ht6)[, c(1, 4)], matrix(FALSE, 5, 2))

  expect_silent(ht7 <- set_bold(ht, everywhere, last_col(), TRUE))
  expect_equivalent(bold(ht7)[, 4], rep(TRUE, 5))
  expect_equivalent(bold(ht7)[, 1:3], matrix(FALSE, 5, 3))

  expect_silent(ht8 <- set_bold(ht, everywhere, last_col(1), TRUE))
  expect_equivalent(bold(ht8)[, 3], rep(TRUE, 5))
  expect_equivalent(bold(ht8)[, c(1, 2, 4)], matrix(FALSE, 5, 3))

  expect_silent(ht9 <- set_bold(ht, everywhere, everything(), TRUE))
  expect_equivalent(bold(ht9), matrix(TRUE, 5, 4))

  ht_nr <- hux(a3 = 1:5, a2 = 1:5, a1 = 1:5, b1 = 1:5)
  expect_silent(ht10 <- set_bold(ht_nr, everywhere, num_range("a", 1:2), TRUE))
  expect_equivalent(bold(ht10)[, 2:3], matrix(TRUE, 5, 2))
  expect_equivalent(bold(ht10)[, c(1, 4)], matrix(FALSE, 5, 2))
})
