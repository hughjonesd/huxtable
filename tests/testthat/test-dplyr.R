local_edition(3)

skip_if_not_installed("dplyr")

test_that("select, rename and relocate", {
  ht <- hux(a = 1:2, b = 1:2, c = 1:2, d = 1:2)
  bold(ht)[1, ] <- TRUE
  ht2 <- dplyr::select(ht, b:c)
  expect_equal(bold(ht2), matrix(c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), 3, 2), ignore_attr = TRUE)
  expect_identical(ht2, ht[, 2:3])
  ht3 <- dplyr::rename(ht, jim = d, bob = c)
  expect_equal(as.vector(colnames(ht3)), c("a", "b", "bob", "jim"))
  expect_equal(bold(ht3), bold(ht), ignore_attr = TRUE)

  if (packageVersion("dplyr") > "0.8.5") {
    ht4 <- dplyr::relocate(ht, b, a, .after = d)
    expect_identical(ht4, ht[c("c", "d", "b", "a")])
    ht5 <- dplyr::relocate(ht, b, a, .before = d)
    expect_identical(ht5, ht[c("c", "b", "a", "d")])
  }
})


test_that("slice, filter, arrange and pull work", {
  ht <- hux(a = 1:4, b = c(1, 3, 4, 2), add_colnames = FALSE)
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


test_that("mutate and transmute work", {
  ht <- hux(a = 1:3, b = 1:3, add_colnames = FALSE)
  bold(ht)[1, ] <- TRUE

  ht2 <- dplyr::mutate(ht, x = a + b)
  expect_equal(as.vector(ht2$x), c(2, 4, 6))
  expect_equal(as.vector(bold(ht2)[, 3]), c(TRUE, FALSE, FALSE))

  ht3 <- dplyr::mutate(ht, x = a + b, copy_cell_props = FALSE)
  expect_equal(as.vector(bold(ht3)[, 3]), c(FALSE, FALSE, FALSE))
  expect_identical(names(ht3), c("a", "b", "x"))

  ht4 <- dplyr::transmute(ht, x = a + b, a = a + b)
  expect_equal(as.vector(ht4$x), c(2, 4, 6))
  expect_equal(as.vector(bold(ht4)[, "a"]), c(TRUE, FALSE, FALSE))

  expect_silent(ht5 <- dplyr::mutate(ht, a = NULL))
  expect_equal(as.vector(dim(font(ht5))), c(3, 1))
})


test_that("set_* works with magrittr pipe", {
  ht_orig <- hux(a = 1:2, b = 1:2)
  expect_silent(ht2 <- ht_orig %>% set_font("times"))
  expect_silent(ht3 <- ht_orig %>% set_all_borders(1))
  expect_equal(font(ht2), matrix("times", 3, 2), ignore_attr = TRUE)
  expect_equal(brdr_thickness(top_border(ht3)), matrix(1, 3, 2), ignore_attr = TRUE)
})


test_that("dplyr select helpers can be used in set_* functions", {
  suppressWarnings(library(tidyselect))
  ht <- hux(a = 1:5, big = 1:5, bog = 1:5, bin = 1:5)

  expect_silent(ht2 <- set_bold(ht, everywhere, starts_with("b"), TRUE))
  expect_equal(bold(ht2)[, -1], matrix(TRUE, 6, 3), ignore_attr = TRUE)
  expect_equal(as.vector(bold(ht2)[, 1]), rep(FALSE, 6))

  expect_silent(ht3 <- set_bold(ht, everywhere, ends_with("g"), TRUE))
  expect_equal(bold(ht3)[, 2:3], matrix(TRUE, 6, 2), ignore_attr = TRUE)
  expect_equal(bold(ht3)[, c(1, 4)], matrix(FALSE, 6, 2), ignore_attr = TRUE)

  expect_silent(ht4 <- set_bold(ht, everywhere, contains("i"), TRUE))
  expect_equal(bold(ht4)[, c(2, 4)], matrix(TRUE, 6, 2), ignore_attr = TRUE)
  expect_equal(bold(ht4)[, c(1, 3)], matrix(FALSE, 6, 2), ignore_attr = TRUE)

  expect_silent(ht5 <- set_bold(ht, everywhere, matches("b[^o]"), TRUE))
  expect_equal(bold(ht5)[, c(2, 4)], matrix(TRUE, 6, 2), ignore_attr = TRUE)
  expect_equal(bold(ht5)[, c(1, 3)], matrix(FALSE, 6, 2), ignore_attr = TRUE)

  col_names <- c("big", "bog")
  expect_silent(ht6 <- set_bold(ht, everywhere, one_of(col_names), TRUE))
  expect_equal(bold(ht6)[, 2:3], matrix(TRUE, 6, 2), ignore_attr = TRUE)
  expect_equal(bold(ht6)[, c(1, 4)], matrix(FALSE, 6, 2), ignore_attr = TRUE)

  expect_silent(ht7 <- set_bold(ht, everywhere, last_col(), TRUE))
  expect_equal(as.vector(bold(ht7)[, 4]), rep(TRUE, 6))
  expect_equal(bold(ht7)[, 1:3], matrix(FALSE, 6, 3), ignore_attr = TRUE)

  expect_silent(ht8 <- set_bold(ht, everywhere, last_col(1), TRUE))
  expect_equal(as.vector(bold(ht8)[, 3]), rep(TRUE, 6))
  expect_equal(bold(ht8)[, c(1, 2, 4)], matrix(FALSE, 6, 3), ignore_attr = TRUE)

  expect_silent(ht9 <- set_bold(ht, everywhere, everything(), TRUE))
  expect_equal(bold(ht9), matrix(TRUE, 6, 4), ignore_attr = TRUE)

  ht_nr <- hux(a3 = 1:5, a2 = 1:5, a1 = 1:5, b1 = 1:5)
  expect_silent(ht10 <- set_bold(ht_nr, everywhere, num_range("a", 1:2), TRUE))
  expect_equal(bold(ht10)[, 2:3], matrix(TRUE, 6, 2), ignore_attr = TRUE)
  expect_equal(bold(ht10)[, c(1, 4)], matrix(FALSE, 6, 2), ignore_attr = TRUE)
})
