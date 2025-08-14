local_edition(3)


test_that("merge_cells", {
  ht <- hux(a = 1:3, b = 1:3, add_colnames = FALSE)  # Keep clean for merge testing
  expect_silent(ht2 <- merge_cells(ht, 1, 1:2))
  expect_silent(ht2 <- merge_cells(ht2, 2:3, 1))
  expect_equal(colspan(ht2), matrix(c(2, 1, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)
  expect_equal(rowspan(ht2), matrix(c(1, 2, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)

  expect_silent(ht3 <- merge_cells(ht, 1, everywhere))
  expect_equal(colspan(ht3), matrix(c(2, 1, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)

  expect_silent(ht4 <- merge_cells(ht, 1:2, 1:2))
  expect_equal(colspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)
  expect_equal(rowspan(ht4), matrix(c(2, 1, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)

  expect_silent(ht5 <- merge_cells(ht, c(1, 3), 1))
  expect_equal(rowspan(ht5), matrix(c(3, 1, 1, 1, 1, 1), 3, 2), ignore_attr = TRUE)
})


test_that("merge_across/down", {
  ht <- hux(1:2, 1:2, add_colnames = FALSE)  # Keep clean for merge testing
  expect_silent(ht2 <- merge_across(ht, 1:2, 1:2))
  expect_equal(colspan(ht2), matrix(c(2, 2, 1, 1), 2, 2), ignore_attr = TRUE)

  expect_silent(ht2 <- merge_down(ht, 1:2, 1:2))
  expect_equal(rowspan(ht2), matrix(c(2, 1, 2, 1), 2, 2), ignore_attr = TRUE)
})


test_that("merge_repeated_rows", {
  ht <- hux(a = c(1, 2, 2), b = c("x", "x", "y"), c = 1:3, add_colnames = FALSE)  # Keep clean for merge testing

  ht2 <- merge_repeated_rows(ht)
  expect_equal(rowspan(ht2), matrix(c(1, 2, 1, 2, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  ht3 <- merge_repeated_rows(ht, everywhere, "a")
  expect_equal(rowspan(ht3), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  ht4 <- merge_repeated_rows(ht, everywhere, 1)
  expect_equal(rowspan(ht4), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  ht5 <- merge_repeated_rows(ht, ht$a > 1, "b")
  expect_equal(rowspan(ht5), matrix(1, 3, 3), ignore_attr = TRUE)

  ht6 <- merge_repeated_rows(ht, ht$a > 1, "a")
  expect_equal(rowspan(ht6), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  ht7 <- merge_repeated_rows(ht, final(2), "b")
  expect_equal(rowspan(ht7), matrix(1, 3, 3), ignore_attr = TRUE)

  ht8 <- merge_repeated_rows(ht, final(2), "a")
  expect_equal(rowspan(ht8), matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  # These should warn about non-contiguous rows (suppressing the warnings from testthat display)
  expect_warning(merge_repeated_rows(ht, ht$c %in% c(1, 3), "a"))
  expect_warning(merge_repeated_rows(ht, c(1, 3), "a"))

  ht9 <- ht
  ht9$b <- as.factor(ht9$b)
  ht9 <- merge_repeated_rows(ht9)
  expect_equal(rowspan(ht9), matrix(c(1, 2, 1, 2, 1, 1, 1, 1, 1), 3, 3), ignore_attr = TRUE)

  ht_long <- hux(a = c(1, 2, 2, 1, 1), add_colnames = FALSE)  # Keep clean for merge testing
  ht_long <- merge_repeated_rows(ht_long)
  expect_equal(rowspan(ht_long), matrix(c(1, 2, 1, 2, 1), 5, 1), ignore_attr = TRUE)
})


test_that("Bug: merge_repeated_rows with NA", {
  ht <- hux(c("a", NA_character_, "b", "c"), add_colnames = FALSE)  # Keep clean for merge testing
  ht <- merge_repeated_rows(ht)
  expect_equal(c(rowspan(ht)), c(1, 1, 1, 1))

  ht2 <- hux(c("a", NA_character_, NA_character_, "b"), add_colnames = FALSE)  # Keep clean for merge testing
  ht2 <- merge_repeated_rows(ht2)
  expect_equal(c(rowspan(ht2)), c(1, 2, 1, 1))

  ht3 <- hux(c(NA_character_, "b", "b"), add_colnames = FALSE)  # Keep clean for merge testing
  ht3 <- merge_repeated_rows(ht3)
  expect_equal(c(rowspan(ht3)), c(1, 2, 1))
})
