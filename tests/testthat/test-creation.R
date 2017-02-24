context("huxtable creation")

## TODO: Rename context
## TODO: Add more tests

test_that("huxtable creation works", {
  hx1 <- huxtable(a = 1:3, b = letters[1:3])
  expect_is(hx1, 'huxtable')
  hx2 <- hux(a = 1:3, b = letters[1:3])
  expect_is(hx2, 'huxtable')
})

test_that('conversion from other formats works', {
  dfr <- data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = FALSE)
  hx3 <- as_huxtable(dfr)
  expect_is(hx3, 'huxtable')
  mat <- matrix(1:6, 3)
  hx4 <- as_huxtable(mat)
  expect_is(hx4, 'huxtable')
  tab <- table(a = rep(1:5, 4), b = rep(1:2, 10))
  hx5 <- as_huxtable(tab)
  expect_is(hx5, 'huxtable')
})
