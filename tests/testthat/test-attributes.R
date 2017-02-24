context("valign")

## TODO: Rename context
## TODO: Add more tests

ht <- huxtable(a = 1:5, b = letters[1:5], d = 1:5)

test_that("valign works", {
  expect_silent(valign(ht)[1, 1] <- 'middle')
  expect_identical(valign(ht)[1, 1], 'middle')
  expect_silent(valign(ht) <- 'top')
  expect_identical(valign(ht)[1, 1], 'top')
  expect_silent(valign(ht)[, 1] <- 'bottom')
  expect_identical(valign(ht)[1, 1], 'bottom')
  expect_silent(valign(ht)[1, ] <- 'top')
  expect_identical(valign(ht)[1, 1], 'top')
  expect_silent(valign(ht)[1, 1] <- NA)
  expect_identical(valign(ht)[1, 1], NA_character_)
})

test_that('valign throws error on bad input', {
  expect_error(valign(ht) <- 'something bad')
})


context("align")

test_that("align works", {
  expect_silent(align(ht)[1, 1] <- 'center')
  expect_identical(align(ht)[1, 1], 'center')
  expect_silent(align(ht) <- 'left')
  expect_identical(align(ht)[1, 1], 'left')
  expect_silent(align(ht)[, 1] <- 'right')
  expect_identical(align(ht)[1, 1], 'right')
  expect_silent(align(ht)[1, ] <- 'left')
  expect_identical(align(ht)[1, 1], 'left')
  expect_silent(align(ht)[1, 1] <- NA)
  expect_identical(align(ht)[1, 1], NA_character_)
})


context("colspan and rowspan")

test_that("colspan and rowspan defaults set", {
  expect_equal(colspan(ht), matrix(1, 5, 3))
  expect_equal(rowspan(ht), matrix(1, 5, 3))
})

test_that("colspan and rowspan setting works", {
  expect_silent( colspan(ht)[1,1] <- 2 )
  expect_identical(colspan(ht)[1,1], 2)
  expect_silent( rowspan(ht)[3,1] <- 2 )
  expect_identical(rowspan(ht)[3,1], 2)
})
