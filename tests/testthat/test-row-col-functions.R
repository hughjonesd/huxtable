
context("Row and column functions")


test_that("final", {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
  expect_equivalent(final(2)(dfr, 1), 4:5)
  expect_equivalent(final(2)(dfr, 2), 3:4)
  expect_equivalent(final(6)(dfr, 1), 1:5)
})


test_that("stripe(), evens and odds", {
  dfr <- data.frame(a = 1:7, b = 1:7, d = 1:7, e = 1:7)
  expect_equivalent(stripe(3, from = 1)(dfr, 1), c(1, 4, 7))
  expect_equivalent(stripe(3, from = 1)(dfr, 2), c(1, 4))
  expect_equivalent(evens(dfr, 1), c(2, 4, 6))
  expect_equivalent(evens(dfr, 2), c(2, 4))
  expect_equivalent(odds(dfr, 1), c(1, 3, 5, 7))
  expect_equivalent(odds(dfr, 2), c(1, 3))
  expect_equivalent(everywhere(dfr, 1), 1:7)
  expect_equivalent(everywhere(dfr, 2), 1:4)
})


test_that("where", {
  dfr <- data.frame(a = 1:3, b = letters[1:3], d = 3:1, stringsAsFactors = FALSE)
  expect_equivalent(where(dfr == 3), matrix(c(3, 1, 1, 3), 2, 2))
})


test_that("where() works with set_*", {
  ht <- hux(a = c(1, 0), b = c(0, 1))
  ht <- set_font(ht, where(ht > 0), "times")
  expect_equivalent(font(ht), matrix(c("times", NA, NA, "times"), 2, 2))
})
