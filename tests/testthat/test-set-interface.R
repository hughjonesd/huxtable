local_edition(3)


test_that("set_* works with variables as arguments", {
  ht_orig <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)  # Keep clean for interface testing
  rownum <- 2
  colnum <- 1
  ht2 <- set_bold(ht_orig, rownum, colnum, TRUE)
  expect_equal(bold(ht2), matrix(c(FALSE, TRUE, FALSE, FALSE), 2, 2), ignore_attr = TRUE)
  boldness <- TRUE
  ht3 <- set_bold(ht_orig, 1:2, 1:2, boldness)
  expect_equal(bold(ht3), matrix(TRUE, 2, 2), ignore_attr = TRUE)
})


test_that("set_* works with logical arguments", {
  ht <- hux(a = 1:2, b = 1:2, add_colnames = FALSE)  # Keep clean for interface testing

  ht2 <- set_bold(ht, 1:2, c(TRUE, FALSE))
  expect_equal(bold(ht2), matrix(c(TRUE, TRUE, FALSE, FALSE), 2, 2), ignore_attr = TRUE)
  ht3 <- set_bold(ht, c(TRUE, FALSE), 1:2)
  expect_equal(bold(ht3), matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2), ignore_attr = TRUE)
  ht4 <- set_bold(ht, c(TRUE, FALSE), c(TRUE, FALSE))
  expect_equal(bold(ht4), matrix(c(TRUE, FALSE, FALSE, FALSE), 2, 2), ignore_attr = TRUE)
})


test_that("set_* works with cell functions", {
  ht <- hux(a = 1:4, b = 1:4, add_colnames = FALSE)  # Keep clean for interface testing
  ht <- set_font(ht, evens, 1:2, "times")
  ht <- set_font(ht, odds, 1:2, "palatino")
  expect_equal(font(ht), matrix(c("palatino", "times"), 4, 2), ignore_attr = TRUE)
  ht <- hux(a = 1:4, b = 1:4, add_colnames = FALSE)  # Keep clean for interface testing
  ht <- set_font(ht, stripe(1), evens, "times")
  ht <- set_font(ht, stripe(1), odds, "palatino")
  expect_equal(font(ht), matrix(c("palatino", "times"), 4, 2, byrow = TRUE), ignore_attr = TRUE)
  ht <- hux(a = 1:4, b = 1:4, add_colnames = FALSE)  # Keep clean for interface testing
  ht <- set_font(ht, stripe(3, from = 1), stripe(1), "times")
  expect_equal(font(ht), matrix(c("times", NA, NA, "times"), 4, 2), ignore_attr = TRUE)
})


test_that("set_* works with row and column functions", {
  ht <- hux(a = 1:4, b = 1:4, add_colnames = FALSE)  # Keep clean for interface testing
  ht <- set_col_width(ht, evens, "20pt")
  ht <- set_col_width(ht, odds, "40pt")
  ht <- set_row_height(ht, evens, "15pt")
  ht <- set_row_height(ht, odds, "30pt")
  expect_equal(as.vector(col_width(ht)), c("40pt", "20pt"))
  expect_equal(as.vector(row_height(ht)), rep(c("30pt", "15pt"), 2))
})


test_that("set_*: 2 argument form", {
  ht <- hux(a = c(1, 0), b = c(0, 1), add_colnames = FALSE)  # Keep clean for interface testing
  ht2 <- set_font(ht, "times")
  expect_equal(font(ht2), matrix("times", 2, 2), ignore_attr = TRUE)
  ht3 <- set_font(ht, value = "times")
  expect_equal(font(ht3), matrix("times", 2, 2), ignore_attr = TRUE)

  ht4 <- set_col_width(ht, c(.6, .4))
  expect_equal(as.vector(col_width(ht4)), c(.6, .4))
  ht5 <- set_row_height(ht, c(.6, .4))
  expect_equal(as.vector(row_height(ht5)), c(.6, .4))
})


test_that("set_* works with row and col 'empty'", {
  ht_orig <- hux(a = c(1, 0), b = c(0, 1), add_colnames = FALSE)
  ht2 <- set_font(ht_orig, 1, everywhere, "times")
  expect_equal(font(ht2), matrix(c("times", NA), 2, 2), ignore_attr = TRUE)
  ht3 <- set_font(ht_orig, everywhere, 1, "times")
  expect_equal(font(ht3), matrix(c("times", "times", NA, NA), 2, 2), ignore_attr = TRUE)
  ht4 <- set_font(ht_orig, "times")
  expect_equal(font(ht4), matrix("times", 2, 2), ignore_attr = TRUE)
})


test_that("set_* default arguments", {
  ht <- hux(a = 1, add_colnames = FALSE)  # Keep clean for interface testing
  expect_silent(ht1 <- set_bold(ht))
  expect_equal(bold(ht1), matrix(TRUE, 1, 1), ignore_attr = TRUE)
  expect_silent(ht2 <- set_bold(ht, 1, 1))
  expect_equal(bold(ht1), matrix(TRUE, 1, 1), ignore_attr = TRUE)
  expect_silent(ht3 <- set_italic(ht))
  expect_equal(italic(ht3), matrix(TRUE, 1, 1), ignore_attr = TRUE)
})


test_that("set_contents works", {
  ht <- hux(1:3, 1:3, add_colnames = FALSE)

  expect_equal(as.matrix(set_contents(ht, 1:6)), as.matrix(hux(1:3, 4:6, add_colnames = FALSE)))
  expect_equal(as.matrix(set_contents(ht, 1, 1, 0)), as.matrix(hux(c(0, 2:3), 1:3, add_colnames = FALSE)))
  expect_equal(as.matrix(set_contents(ht, 1, 1, 0)), as.matrix(hux(c(0, 2:3), 1:3, add_colnames = FALSE)))
  expect_equal(as.matrix(set_contents(ht, 2:3, 2, 3:2)), as.matrix(hux(1:3, c(1, 3, 2), add_colnames = FALSE)))

  ht <- hux(a = 1:3, b = 1:3, add_colnames = FALSE)
  align(ht) <- "right"
  test_props_same <- function(ht2) expect_equal(align(ht2), align(ht), ignore_attr = TRUE)
  test_props_same(set_contents(ht, 1:6))
  test_props_same(set_contents(ht, 1, 1, 0))
  test_props_same(set_contents(ht, 1, 1, 0))
  test_props_same(set_contents(ht, 2:3, 2, 3:2))

  expect_equal(set_contents(ht, 1, "a", 0), set_contents(ht, 1, 1, 0))
  # dplyr::matches not testthat::matches
  skip_if_not_installed("dplyr")
  expect_equal(set_contents(ht, 1, dplyr::matches("b"), 0), set_contents(ht, 1, 2, 0))
})
