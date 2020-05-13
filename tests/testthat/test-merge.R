
test_that("merge_cells", {
  ht <- hux(a = 1:3, b = 1:3)
  expect_silent(ht2 <- merge_cells(ht, 1, 1:2))
  expect_silent(ht2 <- merge_cells(ht2, 2:3, 1))
  expect_equivalent(colspan(ht2), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))
  expect_equivalent(rowspan(ht2), matrix(c(1, 2, 1, 1, 1, 1), 3, 2))

  expect_silent(ht3 <- merge_cells(ht, 1, everywhere))
  expect_equivalent(colspan(ht3), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))

  expect_silent(ht4 <- merge_cells(ht, 1:2, 1:2))
  expect_equivalent(colspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))
  expect_equivalent(rowspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2))

  expect_silent(ht5 <- merge_cells(ht, c(1, 3), 1))
  expect_equivalent(rowspan(ht5), matrix(c(3, 1, 1, 1, 1, 1), 3, 2))

})


test_that("merge_across/down", {
  ht <- hux(1:2, 1:2)
  expect_silent(ht2 <- merge_across(ht, 1:2, 1:2))
  expect_equivalent(colspan(ht2), matrix(c(2, 2, 1, 1), 2, 2))

  expect_silent(ht2 <- merge_down(ht, 1:2, 1:2))
  expect_equivalent(rowspan(ht2), matrix(c(2, 1, 2, 1), 2, 2))
})


test_that("merge_repeated_rows", {
  ht <- hux(a = c(1, 2, 2), b = c("x", "x", "y"), c = 1:3)

  ht2 <- merge_repeated_rows(ht)
  expect_equivalent(rowspan(ht2), matrix(c(1, 2, 1, 2, 1, 1, 1, 1, 1), 3, 3))

  ht3 <- merge_repeated_rows(ht, everywhere, "a")
  expect_equivalent(rowspan(ht3), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3))

  ht4 <- merge_repeated_rows(ht, everywhere, 1)
  expect_equivalent(rowspan(ht4), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3))

  ht5 <- merge_repeated_rows(ht, ht$a > 1, "b")
  expect_equivalent(rowspan(ht5), matrix(1, 3, 3))

  ht6 <- merge_repeated_rows(ht, ht$a > 1, "a")
  expect_equivalent(rowspan(ht6), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3))

  ht7 <- merge_repeated_rows(ht, final(2), "b")
  expect_equivalent(rowspan(ht7), matrix(1, 3, 3))

  ht8 <- merge_repeated_rows(ht, final(2), "a")
  expect_equivalent(rowspan(ht8), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3))

  expect_warning(merge_repeated_rows(ht, ht$c %in% c(1, 3), "a"))
  expect_warning(merge_repeated_rows(ht, c(1, 3), "a"))

  ht9 <- ht
  ht9$b <- as.factor(ht9$b)
  ht9 <- merge_repeated_rows(ht9)
  expect_equivalent(rowspan(ht9), matrix(c(1, 2, 1, 2, 1, 1, 1, 1, 1), 3, 3))

  ht_long <- hux(a = c(1, 2, 2, 1, 1))
  ht_long <- merge_repeated_rows(ht_long)
  expect_equivalent(rowspan(ht_long), matrix(c(1, 2, 1, 2, 1), 5, 1))
})
