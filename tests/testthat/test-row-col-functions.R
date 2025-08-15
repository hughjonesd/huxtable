local_edition(3)


test_that("final", {
  dfr <- data.frame(a = 1:5, b = 1:5, d = 1:5, e = 1:5)
  expect_equal(final(2)(dfr, 1), 4:5)
  expect_equal(final(2)(dfr, 2), 3:4)
  expect_equal(final(6)(dfr, 1), 1:5)
})


test_that("stripe(), evens and odds", {
  dfr <- data.frame(a = 1:7, b = 1:7, d = 1:7, e = 1:7)
  expect_equal(stripe(3, from = 1)(dfr, 1), c(1, 4, 7))
  expect_equal(stripe(3, from = 1)(dfr, 2), c(1, 4))
  expect_equal(evens(dfr, 1), c(2, 4, 6))
  expect_equal(evens(dfr, 2), c(2, 4))
  expect_equal(odds(dfr, 1), c(1, 3, 5, 7))
  expect_equal(odds(dfr, 2), c(1, 3))
  expect_equal(everywhere(dfr, 1), 1:7)
  expect_equal(everywhere(dfr, 2), 1:4)
})
