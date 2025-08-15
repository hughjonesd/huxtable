local_edition(3)


test_that("Subsetting cuts rowspan and colspan", {
  ht <- hux(a = 1:3, b = 1:3, d = 1:3)
  rowspan(ht)[1, 1] <- 3
  colspan(ht)[1, 2] <- 2
  ss <- ht[1:2, 1:2]
  expect_equal(rowspan(ss)[1, 1], 2)
  expect_equal(colspan(ss)[1, 2], 1)
})


test_that("Subsetting works with multirow/multicolumn cells", {
  ht <- hux(a = 1:3, b = 1:3)
  rowspan(ht)[1, 1] <- 2
  expect_silent(ht[c(1, 3), ])
})


test_that("Copying a whole span creates two separate spans", {
  ht <- hux(a = 1:2, b = 1:2)
  rowspan(ht)[1, 1] <- 2
  expect_silent(ht2 <- ht[c(1:2, 1:2), ])
  expect_equal(rowspan(ht2)[1, 1], 2)
  expect_equal(rowspan(ht2)[3, 1], 2)

  ht3 <- hux(a = 1:2, b = 1:2)
  expect_silent(ht4 <- ht3[c(1, 1), ])
  expect_equal(colspan(ht4)[1, 1], 1)
})


test_that("Reordering rows/cols within a span preserves the span unchanged", {
  ht <- hux(a = 1:3, b = 1:3)
  rowspan(ht)[1, 1] <- 3
  expect_silent(ht2 <- ht[c(2, 3, 1), ])
  expect_equal(rowspan(ht2)[1, 1], 3)
})


test_that("Repeating rows/cols within a span, without reordering, extends the span", {
  ht <- hux(a = 1:3, b = 1:3)
  rowspan(ht)[1, 1] <- 2
  expect_silent(ht2 <- ht[c(1, 1, 2, 3), ])
  expect_equal(rowspan(ht2)[1, 1], 3)
})
