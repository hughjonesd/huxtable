
context('huxtable creation')


source('functions.R')


test_that('Object creation examples unchanged', {
  test_ex_same('huxtable')
})


test_that('create huxtable using hux[table]()', {
  expect_silent(ht <- huxtable(a = 1:3, b = 1:3))
  expect_silent(ht2 <- hux(a = 1:3, b = 1:3))
  expect_is(ht, 'huxtable')
  expect_equal(ncol(ht), 2)
  expect_equal(nrow(ht), 3)
  expect_identical(ht, ht2)
})


test_that('create huxtable from data frame', {
  dfr <- data.frame(a = 1:3, b = 1:3)
  expect_silent(ht <- as_hux(dfr))
  expect_silent(ht2 <- as_huxtable(dfr))
  expect_is(ht, 'huxtable')
  expect_identical(ht, huxtable(a = 1:3, b = 1:3))
  expect_identical(ht, ht2)
})


test_that('create huxtable from matrix', {
  m <- matrix(1:8, 4, 2)
  expect_silent(ht <- as_hux(m))
})


test_that('create huxtable from vector', {
  v <- letters[1:5]
  expect_silent(ht <- as_hux(v))
  expect_equal(nrow(ht), length(v))
  nv <- 1:5
  expect_silent(ht <- as_hux(nv))
  expect_equal(nrow(ht), length(nv))
})


test_that('create huxtable from table', {
  tbl <- table(mtcars$gear, mtcars$cyl)
  expect_silent(ht <- as_hux(tbl))
  expect_is(ht, 'huxtable')
  expect_equivalent(ht[[1]][ -1 ], rownames(tbl))
  expect_equivalent(unlist(ht[1, -1]), colnames(tbl))
})


test_that('create huxtable from ftable', {
  ft <- ftable(mtcars[c("cyl", "vs", "gear")])
  expect_silent(ht <- as_hux(ft))
  # the below tests current implementation, where we have separate rows/columns for column and
  # row name names ("dnn")
  expect_equal(nrow(ht), nrow(ft) + length(attr(ft, "col.vars")) + 1)
  expect_equal(ncol(ht), ncol(ft) + length(attr(ft, "row.vars")) + 1)
})
