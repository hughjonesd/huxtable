test_that("left_border", {
  ht <- hux(1:3, 1:3)
  expect_s3_class(left_border(ht), "matrix")
  expect_true(is.matrix(left_border(ht)[1:2, 1:2]))
  expect_type(left_border(ht)[1, 1], "double")
})


test_that("left_border(ht) <- number", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht) <- 1)
  expect_equivalent(left_border(ht)[], matrix(1, 3, 2))
  expect_equivalent(left_border(ht)[1, 1], 1)
})


test_that("left_border(ht) <- bdr(...)", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht) <- bdr(1, "double", "red"))
  expect_equivalent(left_border(ht)[], matrix(1, 3, 2))
  expect_equivalent(left_border_style(ht), matrix("double", 3, 2))
  expect_equivalent(left_border_color(ht), matrix("red", 3, 2))
})


test_that("left_border(ht)[i,j] <-number", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht)[1:2, 1] <- 1)
  expect_equivalent(left_border(ht)[], matrix(c(1, 1, 0, 0, 0, 0), 3, 2))
  expect_equivalent(left_border(ht)[1, 1], 1)
})


test_that("left_border(ht)[i, j] <- bdr(...)", {
  ht <- hux(1:3, 1:3)
  expect_silent(left_border(ht)[1:2, 1] <- bdr(1, "double", "red"))
  expect_equivalent(left_border(ht)[1, 1], 1)
})
